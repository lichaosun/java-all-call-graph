package test.run_by_code.config;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import test.call_graph.annotation.CallMethodWithAnnotation;
import test.call_graph.annotation.MethodWithAnnotation;
import test.call_graph.argument.TestArgument1;
import test.call_graph.argument.TestArgument2;
import test.call_graph.argument.TestArgumentGenerics1;
import test.call_graph.cycle_call.TestCycleCall1;
import test.call_graph.extend_complex.ChildClassA1;
import test.call_graph.extend_complex.ChildClassA2;
import test.call_graph.extend_complex.ChildClassB1;
import test.call_graph.extend_complex.ChildClassB2;
import test.call_graph.extend_complex.TestExtendComplex;
import test.call_graph.interfaces.interfaces.InterfaceSuper1;
import test.call_graph.interfaces.interfaces.InterfaceSuper2;
import test.call_graph.method_call.TestMCCallee;
import test.call_graph.method_call.TestMCCaller;
import test.call_graph.spring.bean.use.complex.TestUseComplexService;
import test.call_graph.spring.mvc.TestSpringController1;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author adrninistrator
 * @date 2023/4/28
 * @description:
 */
public class TestConfigGenerator {

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
}
