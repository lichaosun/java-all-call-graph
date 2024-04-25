package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodAnnotation;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodComment;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringController;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.jacg.util.spring.SpringMvcRequestMappingUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

/**
 * @author slch
 * @date 2024/4/23
 * @description: 写入数据库，方法的注释
 */
@JACGWriteDbHandler(
        readFile = false,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_COMMENT
)
public class WriteDbHandler4MethodComment extends AbstractWriteDbHandler<WriteDbData4MethodComment> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4MethodComment.class);

    @Override
    protected WriteDbData4MethodComment genData(String[] array) {
        // 拆分时限制列数，最后一列注解属性中可能出现空格
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String annotationName = array[1];
        String attributeName = array[2];
        String attributeType = array[3];
        String attributeValue = array[4];

        WriteDbData4MethodComment writeDbData4MethodComment = new WriteDbData4MethodComment();
        writeDbData4MethodComment.setMethodHash(methodHash);
        writeDbData4MethodComment.setAnnotationName(annotationName);
        writeDbData4MethodComment.setAttributeName(attributeName);
        writeDbData4MethodComment.setAnnotationType(attributeType);
        writeDbData4MethodComment.setAttributeValue(attributeValue);
        writeDbData4MethodComment.setFullMethod(fullMethod);
        writeDbData4MethodComment.setSimpleClassName(simpleClassName);
        return writeDbData4MethodComment;
    }
    @Override
    protected Object[] genObjectArray(WriteDbData4MethodComment data) {
        return new Object[]{
                genNextRecordId(),
                data.getMethodHash(),
                data.getAnnotationName(),
                data.getAttributeName(),
                data.getAnnotationType(),
                data.getAttributeValue(),
                data.getFullMethod(),
                data.getSimpleClassName()
        };
    }
}
