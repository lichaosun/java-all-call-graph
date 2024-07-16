package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.MethodDoc;
import com.sun.javadoc.RootDoc;
import org.apache.commons.io.FileUtils;
import org.jboss.forge.roaster.Roaster;
import info.monitorenter.cpdetector.io.*;
import org.jboss.forge.roaster.model.JavaType;
import org.jboss.forge.roaster.model.impl.JavaEnumImpl;
import org.jboss.forge.roaster.model.impl.JavaInterfaceImpl;
import org.jboss.forge.roaster.model.source.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.tools.*;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.JavacTask;
import com.sun.source.util.TreeScanner;
import com.sun.source.util.Trees;
import java.nio.file.Paths;
import java.nio.file.Path;


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
        try {
            File file = new File(filePath);
            if (!file.exists()) {
                logger.error("java file not found: {}", filePath);
            }
            String charset = getFileEncode(file);
            if(null == charset) {
                charset = "gbk";
            }
            com.sun.tools.javadoc.Main.execute(new String[]{"-doclet",
                    JavaDocUtil.class.getName(),
                    "-encoding",
                    charset,
                    "-private",
                    filePath});
            return rootDoc;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
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
            return parseCommentText(javaFilePath);
        }
        Map<String, String> map = new HashMap<>();
        try {
            ClassDoc[] classDocs = rootDoc.classes();
            if (null == classDocs || 0 == classDocs.length) {
                return map;
            }
            int length = classDocs.length;
            String className = classDocs[length - 1].name() + ".java";
            if (!javaFilePath.endsWith(className)) {
                return parseCommentText(javaFilePath);
            }
            for (ClassDoc classDoc : classDocs) {
                MethodDoc[] methodDocs = classDoc.methods();
                for (MethodDoc methodDoc : methodDocs) {
                    String methodName = methodDoc.name();
                    String fullMethod = methodDoc.toString().replace(classDoc + "." + methodName, classDoc + ":" + methodName).replace(" ", "");
                    String methodCommentText = methodDoc.getRawCommentText();
                    String methodKey = fullMethod + JavaCGConstants.FILE_COLUMN_SEPARATOR + JACGCommonNameConstants.JAVADOC_MAIN_ANNOTATION;
                    map.put(methodKey, methodCommentText);
                }
            }
            return map;
        } catch (Exception e) {
            e.printStackTrace();
            return map;
        }
    }

    /**
     * 获取java 类所有方法注释
     *
     * @param javaFilePath
     * @return
     */
    public static Map<String, String> parseCommentText(String javaFilePath) {
        Map<String, String> map = null;
        File file = new File(javaFilePath);
        if (!file.exists()) {
            System.out.println("java file not found: " + javaFilePath);
            return map;
        }
        try {
            String charset = getFileEncode(file);
            if(null == charset) {
                charset = "gbk";
            }
            String javaClassCode = FileUtils.readFileToString(file, charset);
            map = getCommentText(javaClassCode);
        } catch (Exception e) {
            System.out.println("Failed Parser Java File: " + javaFilePath);
            e.printStackTrace();
        }
        return map;
    }


    public static Map<String, String> getCommentText(String javaClassCode) {
        Map<String, String> map = new HashMap<>();
        try {
            JavaType javaType = Roaster.parse(javaClassCode);
            if(javaType instanceof JavaInterfaceImpl) {
                JavaInterfaceImpl javaClassSource = (JavaInterfaceImpl) javaType;
                String packageName = javaClassSource.getPackage();
                String className = javaClassSource.getName();
                List<MethodSource<JavaInterfaceSource>> methodSourceList = javaClassSource.getMethods();
                for (MethodSource<JavaInterfaceSource> methodSource : methodSourceList) {
                    String methodName = methodSource.getName();
                    String params = "";
                    List<ParameterSource<JavaInterfaceSource>> parameterList = methodSource.getParameters();
                    if (null != parameterList && !parameterList.isEmpty()) {
                        int size = parameterList.size();
                        for (int i = 0; i < size - 1; i++) {
                            params = params + parameterList.get(i) + ",";
                        }
                        params = params + parameterList.get(size - 1);
                    }
                    String fullMethod = packageName + "." + className + ":" + methodName + "(" + params + ")";
                    String methodCommentText = methodSource.getJavaDoc().getFullText();
                    String methodKey = fullMethod + JavaCGConstants.FILE_COLUMN_SEPARATOR + JACGCommonNameConstants.FORGE_ROASTER_ANNOTATION;
                    map.put(methodKey, methodCommentText);
                }
            } else if(javaType instanceof JavaEnumImpl) {
                JavaEnumImpl javaClassSource = (JavaEnumImpl) javaType;
                String packageName = javaClassSource.getPackage();
                String className = javaClassSource.getName();
                List<MethodSource<JavaEnumSource>> methodSourceList = javaClassSource.getMethods();
                for (MethodSource<JavaEnumSource> methodSource : methodSourceList) {
                    String methodName = methodSource.getName();
                    String params = "";
                    List<ParameterSource<JavaEnumSource>> parameterList = methodSource.getParameters();
                    if (null != parameterList && !parameterList.isEmpty()) {
                        int size = parameterList.size();
                        for (int i = 0; i < size - 1; i++) {
                            params = params + parameterList.get(i) + ",";
                        }
                        params = params + parameterList.get(size - 1);
                    }
                    String fullMethod = packageName + "." + className + ":" + methodName + "(" + params + ")";
                    String methodCommentText = methodSource.getJavaDoc().getFullText();
                    String methodKey = fullMethod + JavaCGConstants.FILE_COLUMN_SEPARATOR + JACGCommonNameConstants.FORGE_ROASTER_ANNOTATION;
                    map.put(methodKey, methodCommentText);
                }
            } else {
                JavaClassSource javaClassSource = (JavaClassSource) javaType;
                String packageName = javaClassSource.getPackage();
                String className = javaClassSource.getName();
                List<MethodSource<JavaClassSource>> methodSourceList = javaClassSource.getMethods();

                for (MethodSource<JavaClassSource> methodSource : methodSourceList) {
                    String methodName = methodSource.getName();
                    String params = "";
                    List<ParameterSource<JavaClassSource>> parameterList = methodSource.getParameters();
                    if (null != parameterList && !parameterList.isEmpty()) {
                        int size = parameterList.size();
                        for (int i = 0; i < size - 1; i++) {
                            params = params + parameterList.get(i) + ",";
                        }
                        params = params + parameterList.get(size - 1);
                    }
                    String fullMethod = packageName + "." + className + ":" + methodName + "(" + params + ")";
                    String methodCommentText = methodSource.getJavaDoc().getFullText();
                    String methodKey = fullMethod + JavaCGConstants.FILE_COLUMN_SEPARATOR + JACGCommonNameConstants.FORGE_ROASTER_ANNOTATION;
                    map.put(methodKey, methodCommentText);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return map;
    }

    /**
     * 获取文件编码
     *
     * @param document
     * @return
     */
    public static String getFileEncode(File document) {
        CodepageDetectorProxy detector = CodepageDetectorProxy.getInstance();

        detector.add(new ParsingDetector(false));
        detector.add(JChardetFacade.getInstance());
        detector.add(ASCIIDetector.getInstance());
        detector.add(UnicodeDetector.getInstance());
        java.nio.charset.Charset charset = null;
        try {
            charset = detector.detectCodepage(document.toURL());
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (null != charset) {
            return charset.name().toLowerCase();
        }
        return null;
    }

  
}
