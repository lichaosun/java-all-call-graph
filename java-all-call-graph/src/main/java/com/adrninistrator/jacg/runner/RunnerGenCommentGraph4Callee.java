package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.*;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.task.CalleeTaskInfo;
import com.adrninistrator.jacg.dto.task.FindMethodTaskElement;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.File;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author slch
 * @date 2024/5/11
 * @description:
 */
public class RunnerGenCommentGraph4Callee extends AbstractRunnerGenCallGraph {

    private static final Logger logger = LoggerFactory.getLogger(RunnerGenCommentGraph4Callee.class);

    // 生成通用的参数配置
    public static ConfigureWrapper genConfigureWrapper() {
        // java-all-call-graph的配置
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, "test_rbc");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_2.getDetail());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, "20");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, "1000");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, "");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_EXT, ".svg");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_JAVA_DIR, "D:\\workspace\\java-all-call-graph\\java-all-call-graph\\src\\main\\java");

        // H2
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

        // MySQL
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, com.mysql.cj.jdbc.Driver.class.getName());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:mysql://127.0.0.1:3306/jacg?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "root");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "1q2w3e4r");

        /*
            test.jar通过执行以下命令生成：
            gradlew test_jar
         */
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                "build/classes/java"
        );

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX,
                "com.",
                "test.call_graph.",
                "java."
        );

//        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
//                TestMCCallee.class.getName() + ":20",
//                TestMCCallee.class.getName() + ":run(",
//                TestMCCallee.class.getName() + ":run(",
//                System.class.getName(),
//                MethodWithAnnotation.class.getName(),
//                TestArgument1.class.getName(),
//                TestCycleCall1.class.getName()
//        );
//
//        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
//                MethodWithAnnotation.class.getName(),
//                TestMCCaller.class.getName() + ":20",
//                TestArgument1.class.getName() + ":test(",
//                TestArgument2.class.getName() + ":test(",
//                TestArgument2.class.getName() + ":test(",
//                TestArgumentGenerics1.class.getName(),
//                CallMethodWithAnnotation.class.getName() + ":test1(",
//                InterfaceSuper1.class.getName() + ":testSuper1(",
//                InterfaceSuper2.class.getName() + ":testSuper2(",
//                TestCycleCall1.class.getName(),
//                ChildClassA1.class.getName(),
//                ChildClassA2.class.getName(),
//                ChildClassB1.class.getName(),
//                ChildClassB2.class.getName(),
//                TestExtendComplex.class.getName(),
//                test.call_graph.future.CallableImpl.class.getName() + ":call(",
//                TestSpringController1.class.getName(),
//                TestUseComplexService.class.getName()
//        );
//
//        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
//                JACGConstants.CALLEE_FLAG_ENTRY_NO_TAB,
//                JavaCGCommonNameConstants.METHOD_NAME_INIT
//        );
//
//        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
//                System.class.getName(),
//                Deprecated.class.getName()
//        );

        return configureWrapper;
    }

    public static void main(String[] args) throws Exception {
        ConfigureWrapper configureWrapper = genConfigureWrapper();
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb();
        runnerWriteDb.run(configureWrapper);

        Thread.sleep(10000);

        List<String> userstory = new ArrayList<>();
        userstory.add("生成指定数量的问号，使用括号包含");
        userstory.add("启用指定的方法调用");
        userstory.add("根据方法HASH+长度查询查询对应的方法参数泛型类型");

        ThreadPoolExecutor threadPoolExecutor = new ThreadPoolExecutor(4, 10, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<>(10));
        threadPoolExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        for(String story: userstory) {
            threadPoolExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    DbOperWrapper dbOperWrapper = DbOperWrapper.genInstance(configureWrapper, "test");
                    List<String> list = dbOperWrapper.getFullMethodByCommentText(story);
                    if(null != list && !list.isEmpty()) {
                        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, list.toArray(new String[list.size()]));
                    }
                    RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee();
                    runnerGenAllGraph4Callee.run(configureWrapper);

                }
            });
        }
        threadPoolExecutor.shutdown();

//        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller();
//        runnerGenAllGraph4Caller.run(configureWrapper);
    }

    @Override
    protected boolean preHandle() {
        return false;
    }

    @Override
    protected void handle() {
        return;
    }

    private boolean operate() {
        // 创建线程，不指定任务数量，因为在对类进行处理时实际需要处理的方法数无法提前知道
        createThreadPoolExecutor(null);

        // 遍历需要处理的任务
//        for (Map.Entry<String, CalleeTaskInfo> calleeTaskInfoEntry : calleeTaskInfoMap.entrySet()) {
//            // 处理一个被调用类
//            if (!handleOneCalleeClass(calleeTaskInfoEntry)) {
//                // 等待直到任务执行完毕
//                wait4TPEDone();
//                return false;
//            }
//        }

        // 等待直到任务执行完毕
        wait4TPEDone();
        return true;
    }

    private void handleOneCalleeMethod(String entryCalleeSimpleClassName,
                                       FindMethodTaskElement findMethodTaskElement,
                                       String origTaskText) {
        // 等待直到允许任务执行
        JACGUtil.wait4TPEExecute(threadPoolExecutor, taskQueueMaxSize);

        threadPoolExecutor.execute(() -> {
            try {
//                // 执行处理一个被调用方法
//                if (!doHandleOneCalleeMethod(entryCalleeSimpleClassName, findMethodTaskElement, origTaskText)) {
//                    // 记录执行失败的任务信息
//                    recordTaskFail(origTaskText != null ? origTaskText : findMethodTaskElement.getFullMethod());
//                }
            } catch (Exception e) {
                logger.error("error {} ", origTaskText, e);
                // 记录执行失败的任务信息
                recordTaskFail(origTaskText != null ? origTaskText : findMethodTaskElement.getFullMethod());
            }
        });
    }

    private boolean doHandleOneCalleeComment(String commentText, ConfigureWrapper configureWrapper) {
        List<String> fullMethods =  dbOperWrapper.getFullMethodByCommentText(commentText);
        if (null == fullMethods || fullMethods.isEmpty()) {
            logger.info("当前注释没有找到匹配的方法： {}", commentText);
            return true;
        }

        int size = fullMethods.size();
        String[] fullMethodArray = new String[size];
        for(int i=0;i<size;i++) {
            String fullMethod = fullMethods.get(i);
            String[] methodArray = fullMethod.split("\\(");
            fullMethodArray[i] = methodArray[0];
        }
        String subDirName = JavaCGUtil.currentTime() + "_" + UUID.randomUUID().toString();

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, subDirName);
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, fullMethodArray);
        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee();
        boolean flag = runnerGenAllGraph4Callee.run(configureWrapper);

//        List<File> methodOutputFileList = JACGFileUtil.findFileInCurrentDir(currentOutputDirPath + File.separator + JACGConstants.DIR_OUTPUT_METHODS, JACGConstants.EXT_SVG, JACGConstants.EXT_TXT);
//        if (JavaCGUtil.isCollectionEmpty(methodOutputFileList)) {
//            return;
//        }
//
//        String lastClassName = null;
//        List<File> combineMethodFileList = new ArrayList<>();
//        for (File methodOutputFile : methodOutputFileList) {
//            String methodOutputFileName = methodOutputFile.getName();
//            if (methodOutputFileName.endsWith(JACGConstants.EXT_EMPTY_TXT)) {
//                // 跳过空文件
//                continue;
//            }
//
//            // 从方法对应的调用链文件名中获取对应的类名
//            String className = JACGCallGraphFileUtil.getClassNameFromMethodFileName(methodOutputFileName);
//            if (lastClassName != null && !className.equals(lastClassName)) {
//                // 处理到下一个类的文件，合并之前类的文件
//                doCombineClassFile(lastClassName, combineMethodFileList);
//                combineMethodFileList.clear();
//            }
//
//            combineMethodFileList.add(methodOutputFile);
//            lastClassName = className;
//        }
//
//
//
//        Set<String> calleeFiles = JavaCall
//        logger.info("当前方法输出文件名 {}", outputFilePath4Method);
//
//
//
//        if (JACGConstants.EXT_SVG.equals(outputFileType)) {
//            try {
//                // 判断配置文件中是否已指定忽略当前方法
//                if (ignoreCurrentMethod(null, entryCalleeFullMethod)) {
//                    logger.info("配置文件中已指定忽略当前方法，不处理 {}", entryCalleeFullMethod);
//                    return true;
//                }
//
//                // 记录一个被调用方法的调用链信息
//                return recordOneCalleeMethod(entryCalleeSimpleClassName, entryCalleeMethodHash, entryCalleeFullMethod, findMethodTaskElement.getReturnType(),
//                        findMethodTaskElement.getCallFlags(), outputFilePath4Method);
//            } catch (Exception e) {
//                logger.error("error {} {} ", entryCalleeSimpleClassName, outputFilePath4Method, e);
//                return false;
//            }
//        } else {
//            try (BufferedWriter writer4Method = JavaCGFileUtil.genBufferedWriter(outputFilePath4Method)) {
//                // 判断配置文件中是否已指定忽略当前方法
//                if (ignoreCurrentMethod(null, entryCalleeFullMethod)) {
//                    logger.info("配置文件中已指定忽略当前方法，不处理 {}", entryCalleeFullMethod);
//                    return true;
//                }
//
//                // 记录一个被调用方法的调用链信息
//                return recordOneCalleeMethod(entryCalleeSimpleClassName, entryCalleeMethodHash, entryCalleeFullMethod, findMethodTaskElement.getReturnType(),
//                        findMethodTaskElement.getCallFlags(), writer4Method);
//            } catch (Exception e) {
//                logger.error("error {} {} ", entryCalleeSimpleClassName, outputFilePath4Method, e);
//                return false;
//            }
//        }
        return false;
    }


}
