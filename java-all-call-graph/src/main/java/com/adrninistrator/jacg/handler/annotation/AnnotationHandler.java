package com.adrninistrator.jacg.handler.annotation;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.EmptyAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.StringAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.SuperClassWithAnnotation;
import com.adrninistrator.jacg.dto.method.MethodAndHash;
import com.adrninistrator.jacg.extractor.common.enums.SpTxPropagationEnum;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.extends_impl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/6
 * @description: 注解相关的查询处理类
 */
public class AnnotationHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(AnnotationHandler.class);

    private final JACGExtendsImplHandler jacgExtendsImplHandler;

    public AnnotationHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    public AnnotationHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        jacgExtendsImplHandler = new JACGExtendsImplHandler(dbOperWrapper);
    }

    /**
     * 查询带有指定注解的类名列表
     *
     * @param querySimpleClassName true: 查询唯一类名 false: 查询完整类名
     * @param annotationClassNames 注解类名
     * @return
     */
    public List<String> queryClassesWithAnnotations(boolean querySimpleClassName, String... annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return Collections.emptyList();
        }

        SqlKeyEnum sqlKeyEnum = querySimpleClassName ? SqlKeyEnum.CA_QUERY_SIMPLE_CLASS_NAME_WITH_ANNOTATION : SqlKeyEnum.CA_QUERY_CLASS_NAME_WITH_ANNOTATION;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, annotationClassNames.length);
        if (sql == null) {
            sql = "select distinct " + (querySimpleClassName ? DC.CA_SIMPLE_CLASS_NAME : DC.CA_CLASS_NAME) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName() +
                    " where " + DC.CA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, annotationClassNames);
        return JACGSqlUtil.genStringList(list);
    }

    /**
     * 查询带有指定注解的方法列表
     *
     * @param annotationClassNames 指定的注解类名
     * @return
     */
    public List<MethodAndHash> queryMethodsWithAnnotations(String... annotationClassNames) {
        return dbOperWrapper.getMethodsAndHashWithAnnotations(annotationClassNames);
    }

    /**
     * 查询带有指定注解的完整方法列表
     *
     * @param queryFullMethod      true: 查询完整方法 false: 方法HASH+长度
     * @param annotationClassNames 指定的注解类名
     * @return
     */
    public List<String> queryMethodsWithAnnotations(boolean queryFullMethod, String... annotationClassNames) {
        List<MethodAndHash> list = dbOperWrapper.getMethodsAndHashWithAnnotations(annotationClassNames);
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyList();
        }

        List<String> stringList = new ArrayList<>(list.size());
        for (MethodAndHash methodAndHash : list) {
            stringList.add(queryFullMethod ? methodAndHash.getFullMethod() : methodAndHash.getMethodHash());
        }
        return stringList;
    }

    /**
     * 根据完整方法，查询方法指定注解的指定属性
     *
     * @param fullMethod     完整方法
     * @param annotationName 注解类名
     * @param attributeName  注解属性名
     * @param attributeName  注解属性名
     * @return attributeClassType 预期的注解属性类型
     */
    @SuppressWarnings("unchecked")
    public <T extends BaseAnnotationAttribute> T queryAttribute4MethodAnnotation(String fullMethod,
                                                                                 String annotationName,
                                                                                 String attributeName,
                                                                                 Class<T> attributeClassType) {
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        logger.debug("查询方法指定注解的指定属性 {} {} {} {}", fullMethod, methodHash, annotationName, attributeName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_SINGLE_ATTRIBUTE_BY_METHOD_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE, DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_METHOD_HASH + " = ?" +
                    " and " + DC.COMMON_ANNOTATION_ANNOTATION_NAME + " = ?" +
                    " and " + DC.COMMON_ANNOTATION_ATTRIBUTE_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Map<String, Object> map = dbOperator.queryOneRow(sql, new Object[]{methodHash, annotationName, attributeName});
        if (JACGUtil.isMapEmpty(map)) {
            return null;
        }

        // 根据查询的map获取对应的注解属性值
        BaseAnnotationAttribute attribute = genAnnotationAttributeFromMap(map);
        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("方法注解属性的实现类型与预期不一致 {}\n{}\n{}\n{}\n{}", fullMethod, annotationName, attributeName,
                    attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }

        return (T) attribute;
    }

    /**
     * 根据完整方法，及注解类名获取对应的方法的注解信息
     *
     * @param fullMethod
     * @param annotationName
     * @return key：注解属性名称，value：注解属性
     */
    public Map<String, BaseAnnotationAttribute> queryMethodAnnotationAttributes(String fullMethod,
                                                                                String annotationName) {
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        return queryMethodAnnotationAttributes(fullMethod, methodHash, annotationName);
    }

    /**
     * 根据完整方法，及注解类名获取对应的方法的注解信息
     *
     * @param fullMethod     仅用于打印日志
     * @param methodHash
     * @param annotationName
     * @return key：注解属性名称，value：注解属性
     */
    public Map<String, BaseAnnotationAttribute> queryMethodAnnotationAttributes(String fullMethod,
                                                                                String methodHash,
                                                                                String annotationName) {
        logger.debug("查询方法上注解的属性 {} {}", fullMethod, annotationName);
        String sql;
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_ALL_ATTRIBUTES;
        sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE, DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_METHOD_HASH + " = ?" +
                    " and " + DC.COMMON_ANNOTATION_ANNOTATION_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{methodHash, annotationName});
        // 生成记录注解属性的Map
        return genAttributeMapFromList(list);
    }

    // 生成记录注解属性的Map
    private Map<String, BaseAnnotationAttribute> genAttributeMapFromList(List<Map<String, Object>> list) {
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyMap();
        }
        Map<String, BaseAnnotationAttribute> annotationAttributeMap = new HashMap<>(list.size());
        for (Map<String, Object> resultAttributesMap : list) {
            String attributeName = (String) resultAttributesMap.get(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME);
            if (StringUtils.isBlank(attributeName)) {
                // 对于未指定属性的注解，属性名称字段会是""，不需要处理属性值
                annotationAttributeMap.put(attributeName, EmptyAnnotationAttribute.getInstance());
                continue;
            }
            // 根据查询的map获取对应的注解属性值，并记录
            annotationAttributeMap.put(attributeName, genAnnotationAttributeFromMap(resultAttributesMap));
        }
        return annotationAttributeMap;
    }

    /**
     * 查询Spring事务注解@Transactional对应的事务传播行为，仅当确认对应方法上有@Transactional注解时，才能使用当前方法查询
     *
     * @param fullMethod
     * @return
     */
    public String querySpringTxAnnotationPropagation(String fullMethod) {
        StringAnnotationAttribute propagationAttribute = queryAttribute4MethodAnnotation(fullMethod,
                JACGCommonNameConstants.SPRING_TX_ANNOTATION,
                JACGCommonNameConstants.SPRING_TX_ATTRIBUTE_PROPAGATION,
                StringAnnotationAttribute.class);
        if (propagationAttribute == null) {
            return SpTxPropagationEnum.STPE_DEFAULT_REQUIRED.getPropagation();
        }
        return propagationAttribute.getAttributeString();
    }

    /**
     * 获取Spring事务注解@Transactional对应的事务传播行为，仅当确认对应方法上有@Transactional注解时，才能使用当前方法查询
     *
     * @param txPropagationAttribute
     * @return
     */
    public String getSpringTxAnnotationPropagation(BaseAnnotationAttribute txPropagationAttribute) {
        if (txPropagationAttribute == null) {
            return SpTxPropagationEnum.STPE_DEFAULT_REQUIRED.getPropagation();
        }
        return ((StringAnnotationAttribute) txPropagationAttribute).getAttributeString();
    }

    /**
     * 根据完整类名获取对应的注解信息，Map格式
     *
     * @param className 完整类名
     * @return
     */
    public Map<String, Map<String, BaseAnnotationAttribute>> queryAnnotationMap4Class(String className) {
        /*
            返回的Map格式
                key     注解类名
                value   Map<String, BaseAnnotationAttribute> key：注解属性名称，value：注解属性
         */
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        logger.debug("从数据库查询类注解信息 {}", simpleClassName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CA_QUERY_ANNOTATIONS_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ANNOTATION_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE,
                    DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName() +
                    " where " + DC.CA_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> annotationList = dbOperator.queryList(sql, new Object[]{simpleClassName});
        // 根据从数据库的查询结果生成注解对应的Map信息
        return genAnnotationMapFromQueryResult(annotationList);
    }

    /**
     * 获取指定类上指定注解对应的注解属性
     *
     * @param className      完整类名
     * @param annotationName 注解类名
     * @return 若返回map isEmpty()为true，代表类上没有对应的注解。若返回map isEmpty()为false，代表代表类上有对应的注解，key：注解属性名称，value：注解属性
     */
    public Map<String, BaseAnnotationAttribute> queryAnnotationAttributes4Class(String className, String annotationName) {
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        logger.debug("从数据库查询类注解信息 {}", simpleClassName);

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CA_QUERY_ONE_ANNOTATION_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE,
                    DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName() +
                    " where " + DC.CA_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.COMMON_ANNOTATION_ANNOTATION_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> attributeMapList = dbOperator.queryList(sql, new Object[]{simpleClassName, annotationName});
        if (JavaCGUtil.isCollectionEmpty(attributeMapList)) {
            return Collections.emptyMap();
        }

        Map<String, BaseAnnotationAttribute> returnMap = new HashMap<>(attributeMapList.size());
        for (Map<String, Object> attributeMap : attributeMapList) {
            String attributeName = (String) attributeMap.get(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME);
            // 根据查询的map获取对应的注解属性值
            BaseAnnotationAttribute annotationAttribute = genAnnotationAttributeFromMap(attributeMap);
            returnMap.put(attributeName, annotationAttribute);
        }
        return returnMap;
    }

    // 根据从数据库的查询结果生成注解对应的Map信息
    private Map<String, Map<String, BaseAnnotationAttribute>> genAnnotationMapFromQueryResult(List<Map<String, Object>> list) {
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return Collections.emptyMap();
        }
        Map<String, Map<String, BaseAnnotationAttribute>> resultMap = new HashMap<>();
        for (Map<String, Object> map : list) {
            String annotationName = (String) map.get(DC.COMMON_ANNOTATION_ANNOTATION_NAME);
            String attributeName = (String) map.get(DC.COMMON_ANNOTATION_ATTRIBUTE_NAME);

            // 根据查询的map获取对应的注解属性值
            BaseAnnotationAttribute annotationAttribute = genAnnotationAttributeFromMap(map);
            Map<String, BaseAnnotationAttribute> attributeMap = resultMap.computeIfAbsent(annotationName, k -> new HashMap<>());
            attributeMap.put(attributeName, annotationAttribute);
        }
        return resultMap;
    }

    /**
     * 根据完整方法HASH+长度获取对应的方法的注解信息，Map格式
     *
     * @param fullMethod 完整方法
     * @return
     */
    public Map<String, Map<String, BaseAnnotationAttribute>> queryAnnotationMap4FullMethod(String fullMethod) {
        /*
            返回的Map格式
                key     注解类名
                value   Map<String, BaseAnnotationAttribute> key：注解属性名称，value：注解属性
         */
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        logger.debug("从数据库查询方法注解信息 {} {}", fullMethod, methodHash);
        // 查询有方法的注解信息
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_ANNOTATION_BY_METHOD_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MA_METHOD_HASH, DC.COMMON_ANNOTATION_ANNOTATION_NAME, DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE,
                    DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_METHOD_HASH + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        List<Map<String, Object>> annotationList = dbOperator.queryList(sql, new Object[]{methodHash});
        // 根据从数据库的查询结果生成注解对应的Map信息
        return genAnnotationMapFromQueryResult(annotationList);
    }

    /**
     * 从注解属性Map中，根据注解属性名，获取预期类型的注解属性值
     *
     * @param annotationAttributeMap 注解属性Map，key：注解属性名称，value：注解属性
     * @param attributeName          注解属性名
     * @return attributeClassType 预期的注解属性类型
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T extends BaseAnnotationAttribute> T getAttributeFromMap(Map<String, BaseAnnotationAttribute> annotationAttributeMap,
                                                                     String attributeName,
                                                                     Class<T> attributeClassType) {
        if (annotationAttributeMap == null || attributeName == null || attributeClassType == null) {
            return null;
        }

        BaseAnnotationAttribute attribute = annotationAttributeMap.get(attributeName);
        if (attribute == null) {
            logger.debug("注解属性为空 {}", attributeName);
            return null;
        }

        if (!attributeClassType.isAssignableFrom(attribute.getClass())) {
            logger.error("类注解属性的实现类型与预期不一致 {}\n{}\n{}", attributeName, attribute.getClass().getName(), attributeClassType.getName());
            return null;
        }
        return (T) attribute;
    }

    /**
     * 根据查询的map获取对应的注解属性值
     *
     * @param map
     * @return
     */
    private BaseAnnotationAttribute genAnnotationAttributeFromMap(Map<String, Object> map) {
        String attributeValue = (String) map.get(DC.COMMON_ANNOTATION_ATTRIBUTE_VALUE);
        String attributeType = (String) map.get(DC.COMMON_ANNOTATION_ATTRIBUTE_TYPE);
        // 解析注解属性
        return AnnotationAttributesParseUtil.parseFromDb(attributeType, attributeValue);
    }

    /**
     * 查询指定类的所有父类上指定的注解属性
     *
     * @param className      指定类名
     * @param annotationName 指定的注解类名
     * @return
     */
    public List<SuperClassWithAnnotation> querySuperClassesInfo(String className, String annotationName) {
        List<SuperClassWithAnnotation> superClassWithAnnotationList = new ArrayList<>();
        String currentClassName = className;
        while (true) {
            String superClassName = jacgExtendsImplHandler.querySuperClassNameByFull(currentClassName);
            if (superClassName == null) {
                break;
            }
            // 获取指定类上指定注解对应的注解属性
            Map<String, BaseAnnotationAttribute> annotationAttributeMap = queryAnnotationAttributes4Class(superClassName, annotationName);
            SuperClassWithAnnotation superClassWithAnnotation = new SuperClassWithAnnotation(superClassName, annotationAttributeMap);
            superClassWithAnnotationList.add(superClassWithAnnotation);
            // 继续查询当前父类的父类
            currentClassName = superClassName;
        }
        return superClassWithAnnotationList;
    }
}