CREATE TABLE if not exists method_call_{appName} (
  call_id integer NOT NULL COMMENT '调用序号',
  call_type varchar(10) NOT NULL COMMENT '调用类型',
  enabled int NOT NULL COMMENT '是否启用，1:启用',
  caller_jar_num int NOT NULL COMMENT '调用方，Jar包序号',
  caller_method_hash varchar(30) NOT NULL COMMENT '调用方，方法hash+字节数',
  caller_full_method text NOT NULL COMMENT '调用方，完整方法（类名+方法名+参数）',
  caller_method_name varchar(100) NOT NULL COMMENT '调用方，方法名',
  caller_full_class_name varchar(255) NOT NULL COMMENT '调用方，完整类名',
  caller_class_name varchar(255) NOT NULL COMMENT '调用方，类名（全名或简单类名）',
  caller_line_num int NOT NULL COMMENT '调用方，源代码行号',
  callee_method_hash varchar(30) NOT NULL COMMENT '被调用方，方法hash+字节数',
  callee_full_method text NOT NULL COMMENT '被调用方，完整方法（类名+方法名+参数）',
  callee_method_name varchar(100) NOT NULL COMMENT '被调用方，方法名',
  callee_full_class_name varchar(255) NOT NULL COMMENT '被调用方，完整类名',
  callee_class_name varchar(255) NOT NULL COMMENT '被调用方，类名（全名或简单类名）',
  PRIMARY KEY (call_id),
  INDEX idx_mc_rmh_{appName}(caller_method_hash),
  INDEX idx_mc_emh_{appName}(callee_method_hash, caller_method_hash),
  INDEX idx_mc_rsn_{appName}(caller_class_name),
  INDEX idx_mc_esn_{appName}(callee_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用关系表';