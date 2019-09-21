[
{"tag":":const","name":"TST_NAME_MAX_LENGTH","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:8:9","value":16},
{"tag":":const","name":"TST_VERSION","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:7:9","value":1},
{"tag":"unsigned-char","bitSize":8,"bitAlignment":8},
{"tag":"unsigned-short","bitSize":16,"bitAlignment":16},
{"tag":"unsigned-int","bitSize":32,"bitAlignment":32},
{"tag":"unsigned-long","bitSize":32,"bitAlignment":32},
{"tag":"char","bitSize":8,"bitAlignment":8},
{"tag":"short","bitSize":16,"bitAlignment":16},
{"tag":"int","bitSize":32,"bitAlignment":32},
{"tag":"long","bitSize":32,"bitAlignment":32},
{"tag":"struct","id":1,"name":"","location":"/usr/include/bits/types.h:155:12 <Spelling=/usr/include/bits/typesizes.h:72:24>","bitSize":64,"bitAlignment":32,"fields":null},
{"tag":"void","bitSize":null,"bitAlignment":null},
{"tag":"typedef","name":"uint8_t","location":"/usr/include/bits/stdint-uintn.h:24:19","type":{"tag":":unsigned-char"}},
{"tag":"typedef","name":"uint32_t","location":"/usr/include/bits/stdint-uintn.h:26:20","type":{"tag":":unsigned-int"}},
{"tag":"extern","name":"tst_version_string_g","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:10:13","type":{"tag":":array","size":16,"type":{"tag":":char"}}},
{"tag":"enum","id":2,"name":"","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:12:1","fields":[{"name":"tst_black","value":0,"tag":"field"},{"name":"tst_white","value":4294967295,"tag":"field"}]},
{"tag":"struct","id":3,"name":"","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:18:9","bitSize":64,"bitAlignment":64,"fields":null},
{"tag":"struct","id":4,"name":"","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:23:9","bitSize":64,"bitAlignment":64,"fields":null},
{"tag":"typedef","name":"metadata_t","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:25:3","type":{"tag":":struct","id":4,"name":""}},
{"tag":"struct","id":5,"name":"","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:30:3","bitSize":32,"bitAlignment":8,"fields":[{"tag":"field","name":"r","type":{"tag":"uint8_t"},"bitSize":8,"bitAlignment":8,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"g","type":{"tag":"uint8_t"},"bitSize":8,"bitAlignment":8,"bitOffset":8,"bitfield":false,"bitWidth":null},{"tag":"field","name":"b","type":{"tag":"uint8_t"},"bitSize":8,"bitAlignment":8,"bitOffset":16,"bitfield":false,"bitWidth":null},{"tag":"field","name":"a","type":{"tag":"uint8_t"},"bitSize":8,"bitAlignment":8,"bitOffset":24,"bitfield":false,"bitWidth":null}]},
{"tag":"union","id":0,"name":"tst_color_t","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:28:7","bitSize":32,"bitAlignment":32,"fields":[{"tag":"field","name":"encoded","type":{"tag":"uint32_t"},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"component","type":{"tag":":struct","id":5,"name":""},"bitSize":32,"bitAlignment":8,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"array","type":{"tag":":array","size":4,"type":{"tag":"uint8_t"}},"bitSize":32,"bitAlignment":8,"bitOffset":0,"bitfield":false,"bitWidth":null}]},
{"tag":"enum","id":0,"name":"tst_node_kind","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:39:6","fields":[{"name":"tst_node_kind_unknown","value":10,"tag":"field"},{"name":"tst_node_kind_named","value":11,"tag":"field"},{"name":"tst_node_kind_colored","value":20,"tag":"field"}]},
{"tag":"struct","id":0,"name":"tst_named_node_t","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:46:8","bitSize":136,"bitAlignment":8,"fields":[{"tag":"field","name":"name","type":{"tag":":array","size":17,"type":{"tag":":char"}},"bitSize":136,"bitAlignment":8,"bitOffset":0,"bitfield":false,"bitWidth":null}]},
{"tag":"struct","id":0,"name":"tst_colored_node_t","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:50:8","bitSize":32,"bitAlignment":32,"fields":[{"tag":"field","name":"color","type":{"tag":":union","id":0,"name":"tst_color_t"},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null}]},
{"tag":"struct","id":0,"name":"tst_node_t","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:55:8","bitSize":256,"bitAlignment":64,"fields":[{"tag":"field","name":"children","type":{"tag":":pointer","type":{"tag":":struct","id":0,"name":"tst_node_t"}},"bitSize":64,"bitAlignment":64,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"child_count","type":{"tag":":int"},"bitSize":32,"bitAlignment":32,"bitOffset":64,"bitfield":false,"bitWidth":null},{"tag":"field","name":"kind","type":{"tag":":enum","id":0,"name":"tst_node_kind"},"bitSize":32,"bitAlignment":32,"bitOffset":96,"bitfield":false,"bitWidth":null},{"tag":"field","name":"owner","type":{"tag":":pointer","type":{"tag":":struct","id":0,"name":"tst_tree_t"}},"bitSize":64,"bitAlignment":64,"bitOffset":128,"bitfield":false,"bitWidth":null},{"tag":"field","name":"data","type":{"tag":":pointer","type":{"tag":":void"}},"bitSize":64,"bitAlignment":64,"bitOffset":192,"bitfield":false,"bitWidth":null}]},
{"tag":"typedef","name":"tst_node_visitor_t","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:63:16","type":{"tag":":pointer","type":{"tag":":void"}}},
{"tag":"struct","id":9,"name":"","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:67:3","bitSize":32,"bitAlignment":32,"fields":[{"tag":"field","name":"node_count","type":{"tag":":int"},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null}]},
{"tag":"struct","id":0,"name":"tst_tree_t","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:65:16","bitSize":384,"bitAlignment":64,"fields":[{"tag":"field","name":"root","type":{"tag":":struct","id":0,"name":"tst_node_t"},"bitSize":256,"bitAlignment":64,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"info","type":{"tag":":struct","id":9,"name":""},"bitSize":32,"bitAlignment":32,"bitOffset":256,"bitfield":false,"bitWidth":null},{"tag":"field","name":"_meta","type":{"tag":"metadata_t"},"bitSize":64,"bitAlignment":64,"bitOffset":320,"bitfield":false,"bitWidth":null}]},
{"tag":"typedef","name":"tst_tree_t","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:71:3","type":{"tag":":struct","id":0,"name":"tst_tree_t"}},
{"tag":"function","name":"tst_version_string","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:74:7","inline":false,"variadic":false,"returnType":{"tag":":pointer","type":{"tag":":char"}},"storageClass":"none","parameters":null},
{"tag":"function","name":"tst_create_tree","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:76:13","inline":false,"variadic":false,"returnType":{"tag":":pointer","type":{"tag":"tst_tree_t"}},"storageClass":"none","parameters":[{"tag":"parameter","name":"root","type":{"tag":":struct","id":0,"name":"tst_node_t"}}]},
{"tag":"function","name":"tst_destroy_tree","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:78:6","inline":false,"variadic":false,"returnType":{"tag":":void"},"storageClass":"none","parameters":[{"tag":"parameter","name":"tree","type":{"tag":":pointer","type":{"tag":"tst_tree_t"}}}]},
{"tag":"function","name":"tst_create_colored_node","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:80:19","inline":false,"variadic":false,"returnType":{"tag":":struct","id":0,"name":"tst_node_t"},"storageClass":"none","parameters":[{"tag":"parameter","name":null,"type":{"tag":":union","id":0,"name":"tst_color_t"}}]},
{"tag":"function","name":"tst_create_named_node","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:82:19","inline":false,"variadic":false,"returnType":{"tag":":struct","id":0,"name":"tst_node_t"},"storageClass":"none","parameters":[{"tag":"parameter","name":"name","type":{"tag":":array","size":16,"type":{"tag":":char"}}}]},
{"tag":"function","name":"tst_destroy_node","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:84:6","inline":false,"variadic":false,"returnType":{"tag":":void"},"storageClass":"none","parameters":[{"tag":"parameter","name":"node","type":{"tag":":struct","id":0,"name":"tst_node_t"}}]},
{"tag":"function","name":"tst_add_child","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:86:6","inline":false,"variadic":false,"returnType":{"tag":":void"},"storageClass":"none","parameters":[{"tag":"parameter","name":"parent","type":{"tag":":pointer","type":{"tag":":struct","id":0,"name":"tst_node_t"}}},{"tag":"parameter","name":"child","type":{"tag":":struct","id":0,"name":"tst_node_t"}}]},
{"tag":"function","name":"tst_get_node_kind","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:88:20","inline":false,"variadic":false,"returnType":{"tag":":enum","id":0,"name":"tst_node_kind"},"storageClass":"none","parameters":[{"tag":"parameter","name":"node","type":{"tag":":struct","id":0,"name":"tst_node_t"}}]},
{"tag":"function","name":"tst_get_node_color","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:90:19","inline":false,"variadic":false,"returnType":{"tag":":union","id":0,"name":"tst_color_t"},"storageClass":"none","parameters":[{"tag":"parameter","name":"node","type":{"tag":":struct","id":0,"name":"tst_node_t"}}]},
{"tag":"function","name":"tst_set_node_color","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:92:6","inline":false,"variadic":false,"returnType":{"tag":":void"},"storageClass":"none","parameters":[{"tag":"parameter","name":"node","type":{"tag":":pointer","type":{"tag":":struct","id":0,"name":"tst_node_t"}}},{"tag":"parameter","name":null,"type":{"tag":":union","id":0,"name":"tst_color_t"}}]},
{"tag":"function","name":"tst_get_node_name","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:94:7","inline":false,"variadic":false,"returnType":{"tag":":pointer","type":{"tag":":char"}},"storageClass":"none","parameters":[{"tag":"parameter","name":"node","type":{"tag":":struct","id":0,"name":"tst_node_t"}}]},
{"tag":"function","name":"tst_set_node_name","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:96:6","inline":false,"variadic":false,"returnType":{"tag":":void"},"storageClass":"none","parameters":[{"tag":"parameter","name":"node","type":{"tag":":pointer","type":{"tag":":struct","id":0,"name":"tst_node_t"}}},{"tag":"parameter","name":null,"type":{"tag":":pointer","type":{"tag":":char"}}}]},
{"tag":"function","name":"tst_visit_tree_nodes","location":"/home/borodust/devel/repo/bodge-projects/claw/t/c/cffi/../lib/c.h:98:6","inline":false,"variadic":false,"returnType":{"tag":":void"},"storageClass":"none","parameters":[{"tag":"parameter","name":"tree","type":{"tag":":pointer","type":{"tag":"tst_tree_t"}}},{"tag":"parameter","name":"visitor","type":{"tag":"tst_node_visitor_t"}}]}
]