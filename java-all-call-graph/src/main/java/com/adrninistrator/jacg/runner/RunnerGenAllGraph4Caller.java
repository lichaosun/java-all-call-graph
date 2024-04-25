package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.call_graph.CallGraphNode4Caller;
import com.adrninistrator.jacg.dto.call_graph.ChildCallSuperInfo;
import com.adrninistrator.jacg.dto.method.MethodAndHash;
import com.adrninistrator.jacg.dto.task.CallerTaskInfo;
import com.adrninistrator.jacg.dto.task.FindMethodTaskElement;
import com.adrninistrator.jacg.dto.task.FindMethodTaskInfo;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MyBatisMSWriteTable;
import com.adrninistrator.jacg.handler.dto.mybatis.MyBatisMySqlTableInfo;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.dto.stack.ListAsStack;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.attribute.Rank;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.engine.GraphvizCmdLineEngine;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.LinkSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static guru.nidi.graphviz.model.Link.to;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 生成指定方法向下完整调用链
 */

public class RunnerGenAllGraph4Caller extends AbstractRunnerGenCallGraph {
    private static final Logger logger = LoggerFactory.getLogger(RunnerGenAllGraph4Caller.class);

    // 简单类名及对应的完整类名Map
    protected Map<String, String> simpleAndClassNameMap = new ConcurrentHashMap<>();

    // 在一个调用方法中出现多次的被调用方法（包含方法调用业务功能数据），是否需要忽略
    private boolean ignoreDupCalleeInOneCaller;

    @Override
    public boolean preHandle() {
        // 公共预处理
        if (!commonPreHandle()) {
            return false;
        }

        // 读取配置文件中指定的需要处理的任务
        if (!readTaskInfo(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER)) {
            return false;
        }

        // 创建输出文件所在目录
        if (!createOutputDir(JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER)) {
            return false;
        }

        ignoreDupCalleeInOneCaller = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER);
        return true;
    }

    @Override
    public void handle() {
        // 执行实际处理
        if (!operate()) {
            // 记录执行失败的任务信息
            recordTaskFail();
        }
    }

    // 执行实际处理
    private boolean operate() {
        // 生成文件中指定的需要执行的任务信息
        List<CallerTaskInfo> callerTaskInfoList = genCallerTaskInfo();
        if (JavaCGUtil.isCollectionEmpty(callerTaskInfoList)) {
            logger.error("执行失败，请检查配置文件 {} 的内容", OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER);
            return false;
        }

        // 创建线程
        createThreadPoolExecutor(callerTaskInfoList.size());

        // 遍历需要处理的任务，执行任务并等待
        for (CallerTaskInfo callerTaskInfo : callerTaskInfoList) {
            try {
                // 处理一个调用方法
                if (!handleOneCaller(callerTaskInfo)) {
                    // 等待直到任务执行完毕
                    wait4TPEDone();
                    // 记录执行失败的任务信息
                    recordTaskFail(callerTaskInfo.getOrigText());
                }
            } catch (Exception e) {
                logger.error("error {} ", JACGJsonUtil.getJsonStr(callerTaskInfo), e);
                // 记录执行失败的任务信息
                recordTaskFail(callerTaskInfo.getOrigText());
            }
        }

        // 等待直到任务执行完毕
        wait4TPEDone();
        return true;
    }

    // 处理一个调用方法
    private boolean handleOneCaller(CallerTaskInfo callerTaskInfo) {
        String entryCallerSimpleClassName = callerTaskInfo.getCallerSimpleClassName();

        // 获取调用者完整类名
        String entryCallerClassName = getCallerClassName(entryCallerSimpleClassName);
        if (StringUtils.isBlank(entryCallerClassName)) {
            // 生成空文件并返回成功
            return genEmptyFile(callerTaskInfo, entryCallerSimpleClassName, callerTaskInfo.getCallerMethodName());
        }

        FindMethodTaskInfo findMethodTaskInfo;
        if (callerTaskInfo.getCallerMethodName() != null) {
            // 通过方法名称获取调用者方法
            findMethodTaskInfo = findCallerMethodByName(entryCallerClassName, callerTaskInfo);
        } else {
            findMethodTaskInfo = findCallerMethodByLineNumber(callerTaskInfo);
        }

        if (findMethodTaskInfo.isError()) {
            // 出现错误，返回失败
            return false;
        }

        if (findMethodTaskInfo.isGenEmptyFile()) {
            // 已生成空文件，返回成功
            return true;
        }

        List<FindMethodTaskElement> taskElementList = findMethodTaskInfo.getTaskElementList();
        for (FindMethodTaskElement findMethodTaskElement : taskElementList) {
            // 等待直到允许任务执行
            JACGUtil.wait4TPEExecute(threadPoolExecutor, taskQueueMaxSize);
            threadPoolExecutor.execute(() -> {
                // 执行处理一个调用方法
                if (!doHandleOneCaller(callerTaskInfo, entryCallerClassName, findMethodTaskElement)) {
                    // 记录执行失败的任务信息
                    recordTaskFail(callerTaskInfo.getOrigText());
                }
            });
        }
        return true;
    }

    // 执行处理一个调用方法
    private boolean doHandleOneCaller(CallerTaskInfo callerTaskInfo, String entryCallerClassName, FindMethodTaskElement findMethodTaskElement) {
        String entryCallerMethodHash = findMethodTaskElement.getMethodHash();
        String entryCallerFullMethod = findMethodTaskElement.getFullMethod();
        int callFlags = findMethodTaskElement.getCallFlags();
        String entryCallerSimpleClassName = callerTaskInfo.getCallerSimpleClassName();
        int entryLineNumStart = callerTaskInfo.getLineNumStart();
        int entryLineNumEnd = callerTaskInfo.getLineNumEnd();
        logger.info("找到入口方法 {} {}", entryCallerMethodHash, entryCallerFullMethod);

        // 获取当前实际的方法名，而不是使用文件中指定的方法名，文件中指定的方法名可能包含参数，会很长，不可控
        String entryCallerMethodName = JACGClassMethodUtil.getMethodNameFromFull(entryCallerFullMethod);

        // 确定当前方法对应输出文件路径，格式: 配置文件中指定的类名（简单类名或完整类名）+方法名+方法名hash.txt
        StringBuilder outputFilePath = new StringBuilder(currentOutputDirPath).append(File.separator);
        if (callerTaskInfo.getSaveDirPath() != null) {
            outputFilePath.append(callerTaskInfo.getSaveDirPath()).append(File.separator);
            // 创建对应目录
            if (!JACGFileUtil.isDirectoryExists(outputFilePath.toString())) {
                return false;
            }
        }
        // CallerGraphBusinessDataExtractor.genBusinessDataFile()方法中有使用以下文件名格式，不能随便修改
        // 生成方法对应的调用链文件名
        outputFilePath.append(JACGCallGraphFileUtil.getCallGraphMethodFileName(entryCallerSimpleClassName, entryCallerMethodName, entryCallerMethodHash));
        if (entryLineNumStart != JACGConstants.LINE_NUM_NONE && entryLineNumEnd != JACGConstants.LINE_NUM_NONE) {
            // 假如有指定行号时，再加上：@[起始行号]-[结束行号]
            outputFilePath.append(JACGConstants.FLAG_AT).append(entryLineNumStart).append(JACGConstants.FLAG_MINUS).append(entryLineNumEnd);
        }
        if (JACGConstants.EXT_SVG.equals(outputFileType)) {
            outputFilePath.append(JACGConstants.EXT_SVG);
        } else {
            outputFilePath.append(JACGConstants.EXT_TXT);
        }

        String outputFileName = outputFilePath.toString();
        logger.info("当前输出文件名 {} {}", outputFileName, entryCallerFullMethod);

        // 判断文件是否生成过
        if (!writtenFileNameSet.add(outputFileName)) {
            logger.info("当前文件已生成过，不再处理 {} {} {}", callerTaskInfo.getOrigText(), entryCallerFullMethod, outputFileName);
            return true;
        }

        if (JACGConstants.EXT_SVG.equals(outputFileType)) {
            try {
                // 判断配置文件中是否已指定忽略当前方法
                if (ignoreCurrentMethod(null, entryCallerFullMethod)) {
                    logger.info("配置文件中已指定忽略当前方法，不处理 {}", entryCallerFullMethod);
                    return true;
                }

                StringBuilder stringBuilder = new StringBuilder();
                // 在文件第1行写入当前方法的完整信息
                stringBuilder.append(entryCallerFullMethod).append(JavaCGConstants.NEW_LINE);

                // 确定写入输出文件的当前被调用方法信息
                String entryCallerInfo = chooseEntryCallerInfo(entryCallerFullMethod, entryCallerClassName, entryCallerMethodName, entryCallerSimpleClassName,
                        findMethodTaskElement.getReturnType());

                // 第2行写入当前方法的信息
                stringBuilder.append(JACGCallGraphFileUtil.genOutputPrefix(JACGConstants.CALL_GRAPH_METHOD_LEVEL_START)).append(entryCallerInfo);

                // 判断调用方法上是否有注解
                if (MethodCallFlagsEnum.MCFE_ER_METHOD_ANNOTATION.checkFlag(callFlags)) {
                    StringBuilder methodAnnotations = new StringBuilder();
                    // 添加方法注解信息
                    getMethodAnnotationInfo(entryCallerFullMethod, entryCallerMethodHash, methodAnnotations);
                    if (methodAnnotations.length() > 0) {
                        stringBuilder.append(methodAnnotations);
                    }
                }

                if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType())) {
                    // 显示方法参数泛型类型
                    if (!addMethodArgGenericsTypeInfo(true, callFlags, entryCallerMethodHash, stringBuilder)) {
                        return false;
                    }
                }
                if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType())) {
                    // 显示方法返回泛型类型
                    if (!addMethodReturnGenericsTypeInfo(true, callFlags, entryCallerMethodHash, stringBuilder)) {
                        return false;
                    }
                }

                stringBuilder.append(JavaCGConstants.NEW_LINE);

                List<LinkSource> linkSources = new ArrayList<>();

                // 根据指定的调用者方法HASH，查找所有被调用的方法信息
                genAllGraph4Caller(entryCallerMethodHash, entryCallerFullMethod, entryLineNumStart, entryLineNumEnd, linkSources);

                Graph g = graph(entryCallerMethodHash).directed().graphAttr().with(Rank.dir(Rank.RankDir.LEFT_TO_RIGHT)).with(linkSources);
                GraphvizCmdLineEngine graphvizCmdLineEngine = new GraphvizCmdLineEngine();
                graphvizCmdLineEngine.timeout(120, TimeUnit.SECONDS);
                Graphviz.useEngine(graphvizCmdLineEngine);
                Graphviz.fromGraph(g).render(Format.SVG).toFile(new File(outputFileName));
                return true;

            } catch (Exception e) {
                logger.error("error ", e);
                return false;
            }
        } else {
            try (BufferedWriter writer = JavaCGFileUtil.genBufferedWriter(outputFileName)) {
                // 判断配置文件中是否已指定忽略当前方法
                if (ignoreCurrentMethod(null, entryCallerFullMethod)) {
                    logger.info("配置文件中已指定忽略当前方法，不处理 {}", entryCallerFullMethod);
                    return true;
                }

                StringBuilder stringBuilder = new StringBuilder();
                // 在文件第1行写入当前方法的完整信息
                stringBuilder.append(entryCallerFullMethod).append(JavaCGConstants.NEW_LINE);

                // 确定写入输出文件的当前被调用方法信息
                String entryCallerInfo = chooseEntryCallerInfo(entryCallerFullMethod, entryCallerClassName, entryCallerMethodName, entryCallerSimpleClassName,
                        findMethodTaskElement.getReturnType());

                // 第2行写入当前方法的信息
                stringBuilder.append(JACGCallGraphFileUtil.genOutputPrefix(JACGConstants.CALL_GRAPH_METHOD_LEVEL_START)).append(entryCallerInfo);

                // 判断调用方法上是否有注解
                if (MethodCallFlagsEnum.MCFE_ER_METHOD_ANNOTATION.checkFlag(callFlags)) {
                    StringBuilder methodAnnotations = new StringBuilder();
                    // 添加方法注解信息
                    getMethodAnnotationInfo(entryCallerFullMethod, entryCallerMethodHash, methodAnnotations);
                    if (methodAnnotations.length() > 0) {
                        stringBuilder.append(methodAnnotations);
                    }
                }

                if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType())) {
                    // 显示方法参数泛型类型
                    if (!addMethodArgGenericsTypeInfo(true, callFlags, entryCallerMethodHash, stringBuilder)) {
                        return false;
                    }
                }
                if (businessDataTypeSet.contains(DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType())) {
                    // 显示方法返回泛型类型
                    if (!addMethodReturnGenericsTypeInfo(true, callFlags, entryCallerMethodHash, stringBuilder)) {
                        return false;
                    }
                }

                stringBuilder.append(JavaCGConstants.NEW_LINE);
                writer.write(stringBuilder.toString());

                // 根据指定的调用者方法HASH，查找所有被调用的方法信息
                return genAllGraph4Caller(entryCallerMethodHash, entryCallerFullMethod, entryLineNumStart, entryLineNumEnd, writer);
            } catch (Exception e) {
                logger.error("error ", e);
                return false;
            }
        }
    }

    // 生成需要执行的任务信息
    private List<CallerTaskInfo> genCallerTaskInfo() {
        Set<String> handledClassNameSet = new HashSet<>();

        List<CallerTaskInfo> callerTaskInfoList = new ArrayList<>(taskSet.size());
        for (String task : taskSet) {
            if (!StringUtils.containsAny(task, JACGConstants.FLAG_SPACE, JavaCGConstants.FLAG_COLON)) {
                // 当前任务不包含空格或冒号，说明需要将一个类的全部方法添加至任务中
                if (!addAllMethodsInClass2Task(task, handledClassNameSet, callerTaskInfoList)) {
                    return Collections.emptyList();
                }
                continue;
            }

            String left = task;
            int lineNumStart = JACGConstants.LINE_NUM_NONE;
            int lineNumEnd = JACGConstants.LINE_NUM_NONE;

            if (task.contains(JACGConstants.FLAG_SPACE)) {
                String[] array = StringUtils.splitPreserveAllTokens(task, JACGConstants.FLAG_SPACE);
                if (array.length != 2) {
                    logger.error("指定的类名+方法名非法，格式应为 [类名]:[方法名/方法中的代码行号] [起始代码行号]-[结束代码行号] {}", task);
                    return Collections.emptyList();
                }

                left = array[0];
                String right = array[1];
                String[] arrayRight = StringUtils.splitPreserveAllTokens(right, JACGConstants.FLAG_MINUS);
                if (arrayRight.length != 2) {
                    logger.error("指定的行号非法，格式应为 [起始代码行号]-[结束代码行号] {}", task);
                    return Collections.emptyList();
                }

                if (!JavaCGUtil.isNumStr(arrayRight[0]) || !JavaCGUtil.isNumStr(arrayRight[1])) {
                    logger.error("指定的行号非法，应为数字 {}", task);
                    return Collections.emptyList();
                }

                lineNumStart = Integer.parseInt(arrayRight[0]);
                lineNumEnd = Integer.parseInt(arrayRight[1]);
                if (lineNumStart <= 0 || lineNumEnd <= 0) {
                    logger.error("指定的行号非法，应为正整数 {}", task);
                    return Collections.emptyList();
                }

                if (lineNumStart > lineNumEnd) {
                    logger.error("指定的行号非法，起始代码行号不能大于结束代码行号 {}", task);
                    return Collections.emptyList();
                }
            }

            String[] arrayLeft = StringUtils.splitPreserveAllTokens(left, JavaCGConstants.FLAG_COLON);
            if (arrayLeft.length != 2) {
                logger.error("配置文件 {} 中指定的类名+方法名非法\n{}\n格式应为以下之一:\n" +
                                "1. [类名]:[方法名] （代表生成指定类指定名称方法向下的调用链）\n" +
                                "2. [类名]:[方法中的代码行号] （代表生成指定类指定代码行号对应方法向下的调用链）",
                        OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, task);
                return Collections.emptyList();
            }

            String callerClassName = arrayLeft[0];
            String arg2InTask = arrayLeft[1];

            if (StringUtils.isAnyBlank(callerClassName, arg2InTask)) {
                logger.error("指定的类名+方法名存在空值，格式应为 [类名]:[方法名/方法中的代码行号] {}", task);
                return Collections.emptyList();
            }

            // 获取唯一类名（简单类名或完整类名）
            String simpleClassName = getSimpleClassName(callerClassName);
            if (simpleClassName == null) {
                return Collections.emptyList();
            }

            if (handledClassNameSet.contains(simpleClassName)) {
                logger.warn("当前类的全部方法已添加至任务，不需要再指定 {} {}", simpleClassName, task);
                continue;
            }

            CallerTaskInfo callerTaskInfo = new CallerTaskInfo();
            callerTaskInfo.setOrigText(task);
            callerTaskInfo.setCallerSimpleClassName(simpleClassName);
            if (JavaCGUtil.isNumStr(arg2InTask)) {
                // 任务中指定的第二个参数为全数字，作为代码行号处理
                callerTaskInfo.setMethodLineNumber(Integer.parseInt(arg2InTask));
            } else {
                // 任务中指定的第二个参数不是全数字，作为方法名称处理
                callerTaskInfo.setCallerMethodName(arg2InTask);
            }
            callerTaskInfo.setLineNumStart(lineNumStart);
            callerTaskInfo.setLineNumEnd(lineNumEnd);
            callerTaskInfo.setSaveDirPath(null);

            callerTaskInfoList.add(callerTaskInfo);
        }

        return callerTaskInfoList;
    }

    // 将一个类的全部方法添加至任务中
    private boolean addAllMethodsInClass2Task(String className, Set<String> handledClassNameSet, List<CallerTaskInfo> callerTaskInfoList) {
        // 获取唯一类名（简单类名或完整类名）
        String simpleClassName = getSimpleClassName(className);
        if (simpleClassName == null) {
            return false;
        }

        if (!handledClassNameSet.add(simpleClassName)) {
            // 已处理过的类不再处理
            logger.warn("当前类已处理过，不需要再指定 {} {}", simpleClassName, className);
            return true;
        }

        // 查询当前类的所有方法
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLER_ALL_METHODS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLER_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<String> fullMethodList = dbOperator.queryListOneColumn(sql, String.class, simpleClassName);
        if (fullMethodList == null) {
            return false;
        }

        for (String fullMethod : fullMethodList) {
            String methodNameWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(fullMethod);

            CallerTaskInfo callerTaskInfo = new CallerTaskInfo();
            callerTaskInfo.setOrigText(null);
            callerTaskInfo.setCallerSimpleClassName(simpleClassName);
            callerTaskInfo.setCallerMethodName(methodNameWithArgs);
            callerTaskInfo.setLineNumStart(JACGConstants.LINE_NUM_NONE);
            callerTaskInfo.setLineNumEnd(JACGConstants.LINE_NUM_NONE);

            callerTaskInfoList.add(callerTaskInfo);
        }

        return true;
    }

    // 通过方法名称获取调用者方法
    private FindMethodTaskInfo findCallerMethodByName(String callerClassName, CallerTaskInfo callerTaskInfo) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_TOP_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.MC_CALLER_METHOD_HASH, DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_RETURN_TYPE, DC.MC_CALL_FLAGS) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLER_FULL_METHOD +
                    " like concat(?, '%')";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String callerMethodNameInTask = callerTaskInfo.getCallerMethodName();
        String callerSimpleClassName = callerTaskInfo.getCallerSimpleClassName();
        String fullMethodPrefix = JACGClassMethodUtil.getClassAndMethodName(callerClassName, callerMethodNameInTask);
        List<WriteDbData4MethodCall> callerMethodList = dbOperator.queryList(sql, WriteDbData4MethodCall.class, callerSimpleClassName, fullMethodPrefix);
        if (JavaCGUtil.isCollectionEmpty(callerMethodList)) {
            logger.warn("从方法调用关系表未找到指定的调用方法 {} {}", callerSimpleClassName, fullMethodPrefix);
            // 生成空文件
            if (!genEmptyFile(callerTaskInfo, callerSimpleClassName, callerMethodNameInTask)) {
                return FindMethodTaskInfo.genFindMethodInfoFail();
            }
            return FindMethodTaskInfo.genFindMethodInfoGenEmptyFile();
        }

        FindMethodTaskInfo findMethodTaskInfo = FindMethodTaskInfo.genFindMethodInfoSuccess();
        Set<String> handledCallerMethodHashSet = new HashSet<>();

        // 遍历找到的方法
        for (WriteDbData4MethodCall callerMethod : callerMethodList) {
            // 以上查询时包含了call_flags，因此caller_method_hash和caller_full_method可能有重复，需要过滤
            if (handledCallerMethodHashSet.add(callerMethod.getCallerMethodHash())) {
                findMethodTaskInfo.addTaskElement(callerMethod.getCallerMethodHash(), callerMethod.getCallerFullMethod(), callerMethod.getCallFlags(),
                        callerMethod.getCallerReturnType());
            }
        }
        return findMethodTaskInfo;
    }

    // 通过代码行号获取调用者方法
    private FindMethodTaskInfo findCallerMethodByLineNumber(CallerTaskInfo callerTaskInfo) {
        int methodLineNum = callerTaskInfo.getMethodLineNumber();
        if (methodLineNum == 0) {
            logger.error("通过代码行号获取调用者方法时，代码行号为0 {}", JACGJsonUtil.getJsonStr(callerTaskInfo));
            return FindMethodTaskInfo.genFindMethodInfoFail();
        }

        String callerSimpleClassName = callerTaskInfo.getCallerSimpleClassName();
        // 通过代码行号获取对应方法
        FindMethodTaskInfo findMethodTaskInfo = findMethodByLineNumber(false, callerSimpleClassName, methodLineNum);
        if (findMethodTaskInfo.isGenEmptyFile()) {
            // 生成空文件，将代码行号当作方法名使用
            if (!genEmptyFile(callerTaskInfo, callerSimpleClassName, String.valueOf(methodLineNum))) {
                return FindMethodTaskInfo.genFindMethodInfoFail();
            }
        }
        return findMethodTaskInfo;
    }

    // 生成空文件
    private boolean genEmptyFile(CallerTaskInfo callerTaskInfo, String callerClass, String methodName) {
        // 生成空文件并返回成功
        StringBuilder emptyFilePath = new StringBuilder().append(currentOutputDirPath).append(File.separator);
        if (callerTaskInfo.getSaveDirPath() != null) {
            emptyFilePath.append(callerTaskInfo.getSaveDirPath()).append(File.separator);

            // 创建对应目录
            if (!JACGFileUtil.isDirectoryExists(emptyFilePath.toString())) {
                return false;
            }
        }

        // 生成内容为空的调用链文件名
        String finalFilePath = emptyFilePath.append(JACGCallGraphFileUtil.getEmptyCallGraphFileName(callerClass, methodName)).toString();
        logger.info("生成空文件 {} {}", callerClass, finalFilePath);
        // 创建文件
        return JACGFileUtil.createNewFile(finalFilePath);
    }


    /**
     * 根据指定的调用者方法HASH，查找所有被调用方法信息
     *
     * @param entryCallerMethodHash
     * @param entryCallerFullMethod 仅代表调用当前方法时的调用者方法，不代表以下while循环中每次处理到的调用者方法
     * @param entryLineNumStart
     * @param entryLineNumEnd
     * @param writer
     * @return
     */
    protected boolean genAllGraph4Caller(String entryCallerMethodHash,
                                         String entryCallerFullMethod,
                                         int entryLineNumStart,
                                         int entryLineNumEnd,
                                         BufferedWriter writer) throws IOException {
        // 记录当前处理的方法调用信息的栈
        ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack = new ListAsStack<>();
        // 记录子类方法调用父类方法对应信息的栈
        ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack = new ListAsStack<>();

        // 初始加入最上层节点，id设为0（方法调用关系表最小call_id为1）
        CallGraphNode4Caller callGraphNode4CallerHead = new CallGraphNode4Caller(entryCallerMethodHash, JavaCGConstants.METHOD_CALL_ID_START, entryCallerFullMethod);
        callGraphNode4CallerStack.push(callGraphNode4CallerHead);

        // 输出结果数量
        int recordNum = 0;

        // 是否需要显示方法调用被调用方法数
        boolean showCalleeMethodNum;

        // 记录各个层级的调用方法中有被调用过的方法（包含方法注解、方法调用业务功能数据）
        ListAsStack<Set<String>> recordedCalleeStack = null;
        if (ignoreDupCalleeInOneCaller) {
            recordedCalleeStack = new ListAsStack<>();
            // 为第0层的调用者添加Set，不能在以下while循环的if (callGraphNode4CallerStack.atBottom())中添加，因为会执行多次
            recordedCalleeStack.push(new HashSet<>());
        }

        while (true) {
            int lineNumStart = JACGConstants.LINE_NUM_NONE;
            int lineNumEnd = JACGConstants.LINE_NUM_NONE;
            if (callGraphNode4CallerStack.atBottom()) {
                lineNumStart = entryLineNumStart;
                lineNumEnd = entryLineNumEnd;
            }

            // 从栈顶获取当前正在处理的节点
            CallGraphNode4Caller callGraphNode4Caller = callGraphNode4CallerStack.peek();
            // 查询当前节点的一个下层被调用方法
            WriteDbData4MethodCall calleeMethod = queryOneCalleeMethod(callGraphNode4Caller, lineNumStart, lineNumEnd);
            if (calleeMethod == null) {
                // 查询到被调用方法为空时的处理
                if (handleCalleeEmptyResult(callGraphNode4CallerStack, childCallSuperInfoStack, recordedCalleeStack)) {
                    return true;
                }
                continue;
            }

            // 查询到记录，处理输出记录数
            showCalleeMethodNum = handleOutputLineNumber(++recordNum, entryCallerFullMethod);

            String callerFullMethod = callGraphNode4Caller.getCallerFullMethod();
            int methodCallId = calleeMethod.getCallId();
            String callType = calleeMethod.getCallType();
            int enabled = calleeMethod.getEnabled();
            String calleeFullMethod = calleeMethod.getCalleeFullMethod();
            String calleeMethodHash = calleeMethod.getCalleeMethodHash();

            // 处理子类方法调用父类方法的相关信息
            MethodAndHash calleeMethodAndHash = handleChildCallSuperInfo(childCallSuperInfoStack, callGraphNode4CallerStack.getHead(), calleeFullMethod, callerFullMethod,
                    callType, calleeMethodHash);
            if (calleeMethodAndHash == null) {
                // 处理失败
                return false;
            }

            calleeFullMethod = calleeMethodAndHash.getFullMethod();
            calleeMethodHash = calleeMethodAndHash.getMethodHash();

            // 处理被忽略的方法
            if (handleIgnoredMethod(callType, calleeFullMethod, callGraphNode4CallerStack, enabled, methodCallId)) {
                continue;
            }

            // 当前记录需要处理

            // 生成被调用方法信息（包含方法注解信息、方法调用业务功能数据）
            String calleeInfo = genCalleeInfo(calleeFullMethod, calleeMethodHash, calleeMethod.getRawReturnType(), methodCallId, calleeMethod.getCallFlags(), callType);
            if (calleeInfo == null) {
                return false;
            }

            // 若当前被调用方法在调用方法中已被调用过则忽略
            if (ignoreDupCalleeInOneCaller && checkIgnoreDupCalleeInOneCaller(recordedCalleeStack, callGraphNode4CallerStack, calleeInfo, methodCallId)) {
                continue;
            }

            // 处理方法调用的节点信息
            int back2Level = handleCallerNodeInfo(callGraphNode4CallerStack, calleeMethodHash, calleeFullMethod, showCalleeMethodNum);

            // 记录被调用方法信息
            recordCalleeInfo(callerFullMethod, calleeMethod.getCallerLineNumber(), callGraphNode4CallerStack.getHead(), back2Level, calleeInfo, writer);

            // 记录可能出现一对多的方法调用
            if (!recordMethodCallMayBeMulti(methodCallId, callType)) {
                return false;
            }

            // 更新当前处理节点的id
            callGraphNode4CallerStack.peek().setMethodCallId(methodCallId);

            if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
                // 出现循环调用，不再往下处理被调用的方法
                continue;
            }

            // 获取下一层节点
            CallGraphNode4Caller nextCallGraphNode4Caller = new CallGraphNode4Caller(calleeMethodHash, JavaCGConstants.METHOD_CALL_ID_START, calleeFullMethod);
            callGraphNode4CallerStack.push(nextCallGraphNode4Caller);

            // 继续下一层处理
            if (ignoreDupCalleeInOneCaller) {
                // 开始处理下一层，设置对应的Set
                recordedCalleeStack.push(new HashSet<>());
            }
        }
    }

    /**
     * 根据指定的调用者方法HASH，查找所有被调用方法信息
     *
     * @param entryCallerMethodHash
     * @param entryCallerFullMethod 仅代表调用当前方法时的调用者方法，不代表以下while循环中每次处理到的调用者方法
     * @param entryLineNumStart
     * @param entryLineNumEnd
     * @param linkSources
     * @return
     */
    protected boolean genAllGraph4Caller(String entryCallerMethodHash,
                                         String entryCallerFullMethod,
                                         int entryLineNumStart,
                                         int entryLineNumEnd,
                                         List<LinkSource> linkSources) throws IOException {
        // 记录当前处理的方法调用信息的栈
        ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack = new ListAsStack<>();
        // 记录子类方法调用父类方法对应信息的栈
        ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack = new ListAsStack<>();

        // 初始加入最上层节点，id设为0（方法调用关系表最小call_id为1）
        CallGraphNode4Caller callGraphNode4CallerHead = new CallGraphNode4Caller(entryCallerMethodHash, JavaCGConstants.METHOD_CALL_ID_START, entryCallerFullMethod);
        callGraphNode4CallerStack.push(callGraphNode4CallerHead);

        // 输出结果数量
        int recordNum = 0;

        // 是否需要显示方法调用被调用方法数
        boolean showCalleeMethodNum;

        // 记录各个层级的调用方法中有被调用过的方法（包含方法注解、方法调用业务功能数据）
        ListAsStack<Set<String>> recordedCalleeStack = null;
        if (ignoreDupCalleeInOneCaller) {
            recordedCalleeStack = new ListAsStack<>();
            // 为第0层的调用者添加Set，不能在以下while循环的if (callGraphNode4CallerStack.atBottom())中添加，因为会执行多次
            recordedCalleeStack.push(new HashSet<>());
        }

        while (true) {
            int lineNumStart = JACGConstants.LINE_NUM_NONE;
            int lineNumEnd = JACGConstants.LINE_NUM_NONE;
            if (callGraphNode4CallerStack.atBottom()) {
                lineNumStart = entryLineNumStart;
                lineNumEnd = entryLineNumEnd;
            }

            // 从栈顶获取当前正在处理的节点
            CallGraphNode4Caller callGraphNode4Caller = callGraphNode4CallerStack.peek();
            // 查询当前节点的一个下层被调用方法
            WriteDbData4MethodCall calleeMethod = queryOneCalleeMethod(callGraphNode4Caller, lineNumStart, lineNumEnd);
            if (calleeMethod == null) {
                // 查询到被调用方法为空时的处理
                if (handleCalleeEmptyResult(callGraphNode4CallerStack, childCallSuperInfoStack, recordedCalleeStack)) {
                    return true;
                }
                continue;
            }

            // 查询到记录，处理输出记录数
            showCalleeMethodNum = handleOutputLineNumber(++recordNum, entryCallerFullMethod);

            String callerFullMethod = callGraphNode4Caller.getCallerFullMethod();
            int methodCallId = calleeMethod.getCallId();
            String callType = calleeMethod.getCallType();
            int enabled = calleeMethod.getEnabled();
            String calleeFullMethod = calleeMethod.getCalleeFullMethod();
            String calleeMethodHash = calleeMethod.getCalleeMethodHash();

            // 处理子类方法调用父类方法的相关信息
            MethodAndHash calleeMethodAndHash = handleChildCallSuperInfo(childCallSuperInfoStack, callGraphNode4CallerStack.getHead(), calleeFullMethod, callerFullMethod,
                    callType, calleeMethodHash);
            if (calleeMethodAndHash == null) {
                // 处理失败
                return false;
            }

            calleeFullMethod = calleeMethodAndHash.getFullMethod();
            calleeMethodHash = calleeMethodAndHash.getMethodHash();

            // 处理被忽略的方法
            if (handleIgnoredMethod(callType, calleeFullMethod, callGraphNode4CallerStack, enabled, methodCallId)) {
                continue;
            }

            // 当前记录需要处理

            // 生成被调用方法信息（包含方法注解信息、方法调用业务功能数据）
            String calleeInfo = genCalleeInfo(calleeFullMethod, calleeMethodHash, calleeMethod.getRawReturnType(), methodCallId, calleeMethod.getCallFlags(), callType);
            if (calleeInfo == null) {
                return false;
            }

            // 若当前被调用方法在调用方法中已被调用过则忽略
            if (ignoreDupCalleeInOneCaller && checkIgnoreDupCalleeInOneCaller(recordedCalleeStack, callGraphNode4CallerStack, calleeInfo, methodCallId)) {
                continue;
            }

            // 处理方法调用的节点信息
            int back2Level = handleCallerNodeInfo(callGraphNode4CallerStack, calleeMethodHash, calleeFullMethod, showCalleeMethodNum);

            String currentMethodComment = "";
            String parentMethodComment = "";

            List<String> currentCommentList = dbOperWrapper.getCommentTextByFullMethod(callerFullMethod);
            List<String> parentCommentList = dbOperWrapper.getCommentTextByFullMethod(calleeFullMethod);

            if(null != currentCommentList && !currentCommentList.isEmpty()) {
                currentMethodComment = currentCommentList.get(0);
            }

            if(null != parentCommentList && !parentCommentList.isEmpty()) {
                parentMethodComment = parentCommentList.get(0);
            }

            // 记录被调用方法信息
            String callerInfo = JACGClassMethodUtil.getClassMethodNameFromFull(callerFullMethod) + "\n----------------\n" + currentMethodComment;

            LinkSource linkSource = node(callerInfo).link(to(node(calleeInfo+ "\n----------------\n" + parentMethodComment)).with(Label.of("行号：" + calleeMethod.getCallerLineNumber())));
            linkSources.add(linkSource);

            // 记录可能出现一对多的方法调用
            if (!recordMethodCallMayBeMulti(methodCallId, callType)) {
                return false;
            }

            // 更新当前处理节点的id
            callGraphNode4CallerStack.peek().setMethodCallId(methodCallId);

            if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
                // 出现循环调用，不再往下处理被调用的方法
                continue;
            }

            // 获取下一层节点
            CallGraphNode4Caller nextCallGraphNode4Caller = new CallGraphNode4Caller(calleeMethodHash, JavaCGConstants.METHOD_CALL_ID_START, calleeFullMethod);
            callGraphNode4CallerStack.push(nextCallGraphNode4Caller);

            // 继续下一层处理
            if (ignoreDupCalleeInOneCaller) {
                // 开始处理下一层，设置对应的Set
                recordedCalleeStack.push(new HashSet<>());
            }
        }
    }

    /**
     * 处理输出记录数
     *
     * @param outputLineNum
     * @param entryCallerFullMethod
     * @return true: 已达到指定的数量 false: 未达到指定的数量
     */
    private boolean handleOutputLineNumber(int outputLineNum, String entryCallerFullMethod) {
        if (outputLineNum % JACGConstants.NOTICE_LINE_NUM == 0) {
            logger.info("记录数达到 {} {}", outputLineNum, entryCallerFullMethod);
            return true;
        }
        return false;
    }

    // 处理子类方法调用父类方法的相关信息
    private MethodAndHash handleChildCallSuperInfo(ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack,
                                                   int nodeLevel,
                                                   String calleeFullMethod,
                                                   String callerFullMethod,
                                                   String callType,
                                                   String calleeMethodHash) {
        if (JavaCGCallTypeEnum.CTE_CHILD_CALL_SUPER.getType().equals(callType)
                || JavaCGCallTypeEnum.CTE_CHILD_CALL_SUPER_SPECIAL.getType().equals(callType)) {
            // 当前方法调用类型是子类调用父类方法，记录子类方法调用父类方法对应信息的栈入栈
            String callerClassName = JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod);
            String callerSimpleClassName = dbOperWrapper.getSimpleClassName(callerClassName);
            ChildCallSuperInfo childCallSuperInfo = new ChildCallSuperInfo(nodeLevel, callerSimpleClassName, callerClassName, callerFullMethod);
            childCallSuperInfoStack.push(childCallSuperInfo);
            return new MethodAndHash(calleeFullMethod, calleeMethodHash);
        }

        // 获取子类的被调用方法
        Pair<Boolean, MethodAndHash> pair = getCCSChildFullMethod(childCallSuperInfoStack, calleeFullMethod);
        if (Boolean.TRUE.equals(pair.getLeft())) {
            // 使用子类的被调用方法
            return pair.getRight();
        }

        return new MethodAndHash(calleeFullMethod, calleeMethodHash);
    }

    /**
     * 获取子类的被调用方法，若不满足则使用原始方法
     *
     * @param childCallSuperInfoStack
     * @param calleeFullMethod
     * @return left true: 使用子类的被调用方法 false: 使用原始的被调用方法
     * @return right: 子类的被调用方法、方法HASH+长度
     */
    private Pair<Boolean, MethodAndHash> getCCSChildFullMethod(ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack, String calleeFullMethod) {
        // 判断子类方法调用父类方法对应信息的栈是否有数据
        if (childCallSuperInfoStack.isEmpty()) {
            return new ImmutablePair<>(Boolean.FALSE, null);
        }

        String calleeMethodWithArgs = JACGClassMethodUtil.getMethodNameWithArgsFromFull(calleeFullMethod);
        if (calleeMethodWithArgs.startsWith(JavaCGCommonNameConstants.METHOD_NAME_INIT)) {
            // 被调用方法为构造函数，使用原始被调用方法
            return new ImmutablePair<>(Boolean.FALSE, null);
        }

        String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        String calleeSimpleClassName = dbOperWrapper.getSimpleClassName(calleeClassName);

        String ccsChildFullMethod = null;
        String ccsChildMethodHash = null;
        // 保存上一次处理的被调用唯一类名
        String lastChildCallerSimpleClassName = null;
        // 对子类方法调用父类方法对应信息的栈，从栈顶往下遍历
        for (int i = childCallSuperInfoStack.getHead(); i >= 0; i--) {
            ChildCallSuperInfo childCallSuperInfo = childCallSuperInfoStack.getElement(i);
            String childCallerSimpleClassName = childCallSuperInfo.getChildCallerSimpleClassName();

            if (lastChildCallerSimpleClassName != null) {
                if (!jacgExtendsImplHandler.checkExtendsOrImplBySimple(lastChildCallerSimpleClassName, childCallerSimpleClassName)) {
                    // 当前已不是第一次处理，判断上次的子类是否为当前子类的父类，若是则可以继续处理，若否则结束循环
                    break;
                }
                logger.debug("继续处理子类 {} {}", lastChildCallerSimpleClassName, childCallerSimpleClassName);
            }
            lastChildCallerSimpleClassName = childCallerSimpleClassName;

            // 判断子类方法调用父类方法对应信息的栈的调用类（对应子类）是否为当前被调用类的子类
            if (!jacgExtendsImplHandler.checkExtendsOrImplBySimple(calleeSimpleClassName, childCallerSimpleClassName)) {
                // 子类方法调用父类方法对应信息的栈的调用类（对应子类）不是当前被调用类的子类
                break;
            }
            // 子类方法调用父类方法对应信息的栈的调用类为当前被调用类的子类
            String tmpCcsChildFullMethod = JavaCGMethodUtil.formatFullMethodWithArgs(childCallSuperInfo.getChildCallerClassName(), calleeMethodWithArgs);

            // 判断子类方法是否有被调用方法
            SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_METHOD_HASH;
            String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
            if (sql == null) {
                sql = " select " + DC.MI_METHOD_HASH +
                        " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                        " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                        " and " + DC.MI_FULL_METHOD + " like concat(?, '%')" +
                        " limit 1";
                sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
            }

            String tmpCcsChildMethodHash = dbOperator.queryObjectOneColumn(sql, String.class, childCallerSimpleClassName, tmpCcsChildFullMethod);
            if (tmpCcsChildMethodHash == null) {
                // 子类方法不存在，需要继续使用栈中的数据进行处理，可能栈底是子类，栈顶是父类
                continue;
            }

            // 子类方法存在，使用子类方法
            ccsChildFullMethod = tmpCcsChildFullMethod;
            ccsChildMethodHash = tmpCcsChildMethodHash;
        }

        if (ccsChildFullMethod != null) {
            logger.debug("替换子类的向下的方法调用 {} {}", calleeFullMethod, ccsChildFullMethod);
            // 使用子类对应的方法，返回子类方法及子类方法HASH+长度
            return new ImmutablePair<>(Boolean.TRUE, new MethodAndHash(ccsChildFullMethod, ccsChildMethodHash));
        }
        // 使用原始被调用方法
        return new ImmutablePair<>(Boolean.FALSE, null);
    }

    /**
     * 处理被忽略的方法
     *
     * @param callType
     * @param calleeFullMethod
     * @param callGraphNode4CallerStack
     * @param enabled
     * @param methodCallId
     * @return true: 当前方法需要忽略 false: 当前方法不需要忽略
     */
    private boolean handleIgnoredMethod(String callType,
                                        String calleeFullMethod,
                                        ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack,
                                        int enabled,
                                        int methodCallId) {
        if (ignoreCurrentMethod(callType, calleeFullMethod) || !JavaCGYesNoEnum.isYes(enabled)) {
            // 当前记录需要忽略
            // 更新当前处理节点的id
            callGraphNode4CallerStack.peek().setMethodCallId(methodCallId);

            if (!JavaCGYesNoEnum.isYes(enabled)) {
                // 记录被禁用的方法调用
                recordDisabledMethodCall(methodCallId, callType);
            }
            return true;
        }
        return false;
    }

    /**
     * 处理方法调用的节点信息
     * 检查是否出现循环调用
     * 打印当前所有被调用方法对应的被调用方法数
     *
     * @param callGraphNode4CallerStack
     * @param calleeMethodHash
     * @param calleeFullMethod
     * @param showCalleeMethodNum
     * @return -1: 未出现循环调用，非-1: 出现循环调用，值为发生循环调用的层级
     */
    private int handleCallerNodeInfo(ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack,
                                     String calleeMethodHash,
                                     String calleeFullMethod,
                                     boolean showCalleeMethodNum) {
        int cycleCallLevel = JACGConstants.NO_CYCLE_CALL_FLAG;
        // 方法调用被调用方法数信息
        StringBuilder calleeMethodNumLogInfo = null;
        if (showCalleeMethodNum) {
            calleeMethodNumLogInfo = new StringBuilder();
        }

        // 循环调用的日志信息
        StringBuilder cycleCallLogInfo = new StringBuilder();

        // 遍历每个层级的被调用方法
        for (int i = 0; i <= callGraphNode4CallerStack.getHead(); i++) {
            CallGraphNode4Caller callGraphNode4Caller = callGraphNode4CallerStack.getElement(i);
            // 当前层级的被调用方法的被调用方法数加1
            callGraphNode4Caller.addCallerMethodNum();

            if (cycleCallLevel == JACGConstants.NO_CYCLE_CALL_FLAG && calleeMethodHash.equals(callGraphNode4Caller.getCallerMethodHash())) {
                // 找到循环调用
                cycleCallLevel = i;
            }

            if (cycleCallLevel != JACGConstants.NO_CYCLE_CALL_FLAG) {
                // 记录循环调用信息
                if (cycleCallLogInfo.length() > 0) {
                    cycleCallLogInfo.append("\n");
                }
                cycleCallLogInfo.append(JACGCallGraphFileUtil.genOutputLevelFlag(i))
                        .append(" ")
                        .append(callGraphNode4Caller.getCallerFullMethod());
            }

            if (showCalleeMethodNum) {
                // 记录方法调用被调用方法数
                if (calleeMethodNumLogInfo.length() > 0) {
                    calleeMethodNumLogInfo.append("\n");
                }
                calleeMethodNumLogInfo.append(JACGCallGraphFileUtil.genOutputLevelFlag(i))
                        .append(" 被调用方法数:").append(callGraphNode4Caller.getCallerMethodNum()).append(" ")
                        .append(callGraphNode4Caller.getCallerFullMethod());
            }
        }

        // 每个层级的被调用方法遍历完之后的处理
        if (cycleCallLevel != JACGConstants.NO_CYCLE_CALL_FLAG) {
            // 显示被循环调用的信息
            cycleCallLogInfo.append("\n")
                    .append(JACGCallGraphFileUtil.genCycleCallFlag(cycleCallLevel))
                    .append(" ")
                    .append(calleeFullMethod);
            logger.info("找到循环调用的方法\n{}", cycleCallLogInfo);
        }

        if (showCalleeMethodNum) {
            // 显示方法调用被调用方法数
            logger.info("被调用方法数（当前方法向下会调用的方法数量）\n{}", calleeMethodNumLogInfo);
        }
        return cycleCallLevel;
    }

    // 查询当前节点的一个下层被调用方法
    private WriteDbData4MethodCall queryOneCalleeMethod(CallGraphNode4Caller callGraphNode4Caller, int lineNumStart, int lineNumEnd) {
        // 判断查询时是否使用代码行号
        boolean useLineNum = lineNumStart != JACGConstants.LINE_NUM_NONE && lineNumEnd != JACGConstants.LINE_NUM_NONE;
        SqlKeyEnum sqlKeyEnum = useLineNum ? SqlKeyEnum.MC_QUERY_ONE_CALLEE_CHECK_LINE_NUM : SqlKeyEnum.MC_QUERY_ONE_CALLEE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            // 确定查询被调用关系时所需字段
            sql = "select " + chooseCalleeColumns() +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALL_ID + " > ?";
            if (useLineNum) {
                sql = sql + " and " + DC.MC_CALLER_LINE_NUMBER + " >= ? and " + DC.MC_CALLER_LINE_NUMBER + " <= ?";
            }
            sql = sql + " order by " + DC.MC_CALL_ID +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> argList = new ArrayList<>(4);
        argList.add(callGraphNode4Caller.getCallerMethodHash());
        argList.add(callGraphNode4Caller.getMethodCallId());
        if (lineNumStart != JACGConstants.LINE_NUM_NONE && lineNumEnd != JACGConstants.LINE_NUM_NONE) {
            argList.add(lineNumStart);
            argList.add(lineNumEnd);
        }

        return dbOperator.queryObject(sql, WriteDbData4MethodCall.class, argList.toArray());
    }

    /**
     * 查询到被调用方法为空时的处理
     *
     * @param callGraphNode4CallerStack
     * @param childCallSuperInfoStack
     * @param recordedCalleeStack
     * @return true: 需要结束循环 false: 不结束循环
     */
    private boolean handleCalleeEmptyResult(ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack,
                                            ListAsStack<ChildCallSuperInfo> childCallSuperInfoStack,
                                            ListAsStack<Set<String>> recordedCalleeStack) {
        if (callGraphNode4CallerStack.atBottom()) {
            // 当前处理的节点为最上层节点，结束循环
            return true;
        }

        // 当前处理的节点不是最上层节点，返回上一层处理
        if (ignoreDupCalleeInOneCaller) {
            // 清空不再使用的下一层Set
            recordedCalleeStack.removeTop();
        }

        if (!childCallSuperInfoStack.isEmpty()) {
            // 记录子类方法调用父类方法对应信息的栈非空
            ChildCallSuperInfo topChildCallSuperInfo = childCallSuperInfoStack.peek();
            if (topChildCallSuperInfo.getChildCallerNodeLevel() == callGraphNode4CallerStack.getHead()) {
                // 记录子类方法调用父类方法对应信息的栈顶元素，与方法调用节点栈出栈的级别相同，出栈
                childCallSuperInfoStack.removeTop();
            }
        }

        // 删除栈顶元素
        callGraphNode4CallerStack.removeTop();

        return false;
    }

    // 生成被调用方法信息（包含方法注解信息、方法调用业务功能数据）
    protected String genCalleeInfo(String calleeFullMethod,
                                   String calleeMethodHash,
                                   String calleeReturnType,
                                   int methodCallId,
                                   int callFlags,
                                   String callType) {
        StringBuilder calleeInfo = new StringBuilder();
        String calleeClassName = null;
        String calleeMethodName = null;
        if (OutputDetailEnum.ODE_0 == outputDetailEnum) {
            // # 0: 展示 完整类名+方法名+方法参数+返回类型
            calleeInfo.append(JACGClassMethodUtil.genFullMethodWithReturnType(calleeFullMethod, calleeReturnType));
        } else if (OutputDetailEnum.ODE_1 == outputDetailEnum) {
            // # 1: 展示 完整类名+方法名+方法参数
            calleeInfo.append(calleeFullMethod);
        } else if (OutputDetailEnum.ODE_2 == outputDetailEnum) {
            // # 2: 展示 完整类名+方法名
            calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
            calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
            calleeInfo.append(calleeClassName)
                    .append(JavaCGConstants.FLAG_COLON)
                    .append(calleeMethodName);
        } else {
            // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
            calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
            String calleeSimpleClassName = dbOperWrapper.getSimpleClassName(calleeClassName);
            calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
            calleeInfo.append(calleeSimpleClassName)
                    .append(JavaCGConstants.FLAG_COLON)
                    .append(calleeMethodName);
        }

        // 判断被调用方法上是否有注解
        Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap = null;
        if (MethodCallFlagsEnum.MCFE_EE_METHOD_ANNOTATION.checkFlag(callFlags)) {
            StringBuilder methodAnnotations = new StringBuilder();
            // 添加方法注解信息
            methodAnnotationMap = getMethodAnnotationInfo(calleeFullMethod, calleeMethodHash, methodAnnotations);
            if (methodAnnotations.length() > 0) {
                calleeInfo.append(methodAnnotations);
            }
        }

        // 添加方法调用业务功能数据
        if (!addBusinessData(methodCallId, callFlags, calleeMethodHash, calleeInfo)) {
            return null;
        }

        if (calleeClassName == null) {
            calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        }
        if (calleeMethodName == null) {
            calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
        }
        // 为向下的方法完整调用链添加默认的方法调用业务功能数据
        if (!addDefaultBusinessData4er(callFlags, calleeClassName, calleeMethodName, calleeInfo)) {
            return null;
        }

        // 为方法调用信息增加是否在其他线程执行标志
        addRunInOtherThread(calleeInfo, methodCallId, callType, methodAnnotationMap);

        // 为方法调用信息增加是否在事务中执行标志
        addRunInTransaction(calleeInfo, methodCallId, callType, methodAnnotationMap);

        return calleeInfo.toString();
    }

    /**
     * 若当前被调用方法在调用方法中已被调用过则忽略
     *
     * @param recordedCalleeStack
     * @param callGraphNode4CallerStack
     * @param calleeInfo
     * @param methodCallId
     * @return true: 需要忽略 false:不忽略
     */
    private boolean checkIgnoreDupCalleeInOneCaller(ListAsStack<Set<String>> recordedCalleeStack,
                                                    ListAsStack<CallGraphNode4Caller> callGraphNode4CallerStack,
                                                    String calleeInfo,
                                                    int methodCallId) {
        Set<String> callerRecordedCalleeSet = recordedCalleeStack.peek();
        if (!callerRecordedCalleeSet.add(calleeInfo)) {
            // 当前被调用方法在调用方法中已被调用过，忽略
            logger.debug("忽略一个方法中被调用多次的方法 {} {}", callGraphNode4CallerStack.getHead(), calleeInfo);

            // 更新当前处理节点的id
            callGraphNode4CallerStack.peek().setMethodCallId(methodCallId);
            return true;
        }
        return false;
    }

    // 记录被调用方法信息
    protected void recordCalleeInfo(String callerFullMethod,
                                    int callerLineNumber,
                                    int currentNodeLevel,
                                    int back2Level,
                                    String calleeInfo,
                                    BufferedWriter writer) throws IOException {
        StringBuilder stringBuilder = new StringBuilder();
        // 生成输出文件前缀，包含了当前方法的调用层级
        String prefix = JACGCallGraphFileUtil.genOutputPrefix(currentNodeLevel + 1);

        // 写入前缀：    "[2]#    "
        stringBuilder.append(prefix);

        String callerClassName = JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String callerSimpleClassName = dbOperWrapper.getSimpleClassName(callerClassName);

        // 写入调用者行号信息：   "[Service1Impl:29]	"
        // 显示调用者代码行号
        stringBuilder.append(JACGConstants.FLAG_LEFT_PARENTHESES).
                append(callerSimpleClassName).
                append(JavaCGConstants.FLAG_COLON).
                append(callerLineNumber).
                append(JACGConstants.FLAG_RIGHT_PARENTHESES).
                append(JACGConstants.FLAG_TAB);

        // 写入被调用者信息（包含方法注解信息、方法调用业务功能数据）：    test.example.service.impl.Service1Impl:test1
        stringBuilder.append(calleeInfo);

        // 写入循环调用标志
        if (back2Level != JACGConstants.NO_CYCLE_CALL_FLAG) {
            stringBuilder.append(JACGConstants.FLAG_TAB).append(JACGCallGraphFileUtil.genCycleCallFlag(back2Level));
        }

        // 写入换行符
        stringBuilder.append(JACGConstants.NEW_LINE);
        writer.write(stringBuilder.toString());
    }

    // 确定写入输出文件的当前调用方法信息
    private String chooseEntryCallerInfo(String callerFullMethod, String callerClassName, String callerMethodName, String callerSimpleClassName, String callerReturnType) {
        if (OutputDetailEnum.ODE_0 == outputDetailEnum) {
            // # 0: 展示 完整类名+方法名+方法参数+返回类型
            return JACGClassMethodUtil.genFullMethodWithReturnType(callerFullMethod, callerReturnType);
        }
        if (OutputDetailEnum.ODE_1 == outputDetailEnum) {
            // # 1: 展示 完整类名+方法名+方法参数
            return callerFullMethod;
        } else if (OutputDetailEnum.ODE_2 == outputDetailEnum) {
            // # 2: 展示 完整类名+方法名
            return JACGClassMethodUtil.getClassAndMethodName(callerClassName, callerMethodName);
        }
        // # 3: 展示 简单类名（对于同名类展示完整类名）+方法名
        return JACGClassMethodUtil.getClassAndMethodName(callerSimpleClassName, callerMethodName);
    }

    // 确定查询调用关系时所需字段
    private String chooseCalleeColumns() {
        return JACGSqlUtil.joinColumns(
                DC.MC_CALL_ID,
                DC.MC_CALL_TYPE,
                DC.MC_ENABLED,
                DC.MC_CALLEE_FULL_METHOD,
                DC.MC_CALLEE_METHOD_HASH,
                DC.MC_CALLER_LINE_NUMBER,
                DC.MC_CALL_FLAGS,
                DC.MC_RAW_RETURN_TYPE
        );
    }

    // 获取调用者完整类名
    private String getCallerClassName(String callerSimpleClassName) {
        String existedClassName = simpleAndClassNameMap.get(callerSimpleClassName);
        if (existedClassName != null) {
            return existedClassName;
        }

        // 根据简单类名，查找对应的完整方法
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLER_FULL_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_SIMPLE_CLASS_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String fullMethod = dbOperator.queryObjectOneColumn(sql, String.class, callerSimpleClassName);
        if (fullMethod == null) {
            logger.warn("从方法调用关系表未找到对应的完整类名 {}", callerSimpleClassName);
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        simpleAndClassNameMap.putIfAbsent(callerSimpleClassName, className);
        return className;
    }

    // 为向下的方法完整调用链添加默认的方法调用业务功能数据
    private boolean addDefaultBusinessData4er(int callFlags,
                                              String calleeClassName,
                                              String calleeMethodName,
                                              StringBuilder callGraphInfo) {
        for (String businessDataType : businessDataTypeList) {
            if (DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType().equals(businessDataType)) {
                // 显示MyBatis的XML文件中对应的数据库表名
                if (!MethodCallFlagsEnum.MCFE_EE_MYBATIS_MAPPER.checkFlag(callFlags)) {
                    continue;
                }

                MyBatisMySqlTableInfo myBatisMySqlTableInfo = myBatisMapperHandler.queryMyBatisMySqlTableInfo(calleeClassName, calleeMethodName);
                if (myBatisMySqlTableInfo == null) {
                    return false;
                }
                addBusinessData2CallGraphInfo(businessDataType, myBatisMySqlTableInfo, callGraphInfo);
            } else if (DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_WRITE_TABLE.getType().equals(businessDataType)) {
                // 显示MyBatis的XML文件中对应的写数据库表名
                if (!MethodCallFlagsEnum.MCFE_EE_MYBATIS_MAPPER_WRITE.checkFlag(callFlags)) {
                    continue;
                }

                WriteDbData4MyBatisMSWriteTable writeDbData4MyBatisMSWriteTable = myBatisMapperHandler.queryMyBatisMySqlWriteTableInfo(calleeClassName, calleeMethodName);
                if (writeDbData4MyBatisMSWriteTable == null) {
                    return false;
                }
                addBusinessData2CallGraphInfo(businessDataType, writeDbData4MyBatisMSWriteTable, callGraphInfo);
            }
        }

        return true;
    }
}
