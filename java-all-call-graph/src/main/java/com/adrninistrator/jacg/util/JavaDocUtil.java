package com.adrninistrator.jacg.util;

import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.MethodDoc;
import com.sun.javadoc.Parameter;
import com.sun.javadoc.RootDoc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * @author slch
 * @date 2024/4/23
 * @description: JavaDoc 工具类
 */

public class JavaDocUtil {
    private static final Logger logger = LoggerFactory.getLogger(JavaDocUtil.class);
    private static RootDoc rootDoc;

    /**
     * JavaDoc 必须要实现
     *
     * @param root
     * @return
     */
    public static boolean start(RootDoc root) {
        rootDoc = root;
        return true;
    }

    /**
     * 获取 rootDoc
     *
     * @param filePath
     * @return
     */
    public static RootDoc getRootDoc(String filePath) {
        File file = new File(filePath);
        if (!file.exists()) {
            logger.error("java file not found: {}", filePath);
        }
        com.sun.tools.javadoc.Main.execute(new String[]{"-doclet",
                JavaDocUtil.class.getName(),
                "-docletpath",
                JavaDocUtil.class.getResource("/").getPath(),
                "-encoding", "utf-8",
                filePath});
        return rootDoc;
    }

    /**
     * 获取java 类所有方法注释
     *
     * @param javaFilePath
     * @return
     */
    public static Map<String, String> parseRawCommentText(String javaFilePath) {
        RootDoc rootDoc = getRootDoc(javaFilePath);
        if (null == rootDoc) {
            return null;
        }
        Map<String, String> map = new HashMap<>();
        ClassDoc[] classDocs = rootDoc.classes();
        for (ClassDoc classDoc : classDocs) {
            MethodDoc[] methodDocs = classDoc.methods();
            for (MethodDoc methodDoc : methodDocs) {
                String methodName = methodDoc.name();
                String fullMethod = methodDoc.toString().replace(classDoc + "." + methodName, classDoc + ":" + methodName).replace(" ", "");
                String methodCommentText = methodDoc.getRawCommentText();
                map.put(fullMethod, methodCommentText);
            }
        }
        return map;
    }

    public static void main(String[] args) {
        String javaFilePath = "D:\\code\\java-all-call-graph\\java-all-call-graph\\src\\main\\java\\com\\adrninistrator\\jacg\\util\\JACGUtil.java";
        parseRawCommentText(javaFilePath);
    }
}
