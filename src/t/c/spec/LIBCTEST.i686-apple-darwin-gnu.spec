[
{"tag":"int","bitSize":32,"bitAlignment":32},
{"tag":"char","bitSize":8,"bitAlignment":8},
{"tag":"struct","id":0,"name":"s_a","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:5:8","bitSize":160,"bitAlignment":32,"fields":[{"tag":"field","name":"pointer","type":{"tag":":pointer","type":{"tag":":struct","id":0,"name":"s_a"}},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"array","type":{"tag":":array","size":16,"type":{"tag":":char"}},"bitSize":128,"bitAlignment":8,"bitOffset":32,"bitfield":false,"bitWidth":null}]},
{"tag":"float","bitSize":32,"bitAlignment":32},
{"tag":"struct","id":2,"name":"","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:11:3","bitSize":32,"bitAlignment":32,"fields":[{"tag":"field","name":"enclosed_field","type":{"tag":":float"},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null}]},
{"tag":"double","bitSize":64,"bitAlignment":32},
{"tag":"struct","id":0,"name":"s_b","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:10:16","bitSize":256,"bitAlignment":32,"fields":[{"tag":"field","name":"record_field","type":{"tag":":struct","id":2,"name":""},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"plain_field","type":{"tag":":double"},"bitSize":64,"bitAlignment":32,"bitOffset":32,"bitfield":false,"bitWidth":null},{"tag":"field","name":"another_record_field","type":{"tag":":struct","id":0,"name":"s_a"},"bitSize":160,"bitAlignment":32,"bitOffset":96,"bitfield":false,"bitWidth":null}]},
{"tag":"typedef","name":"s_b","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:16:3","type":{"tag":":struct","id":0,"name":"s_b"}},
{"tag":"enum","id":0,"name":"e_a","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:18:6","fields":[{"name":"c_0","value":10,"tag":"field"},{"name":"c_1","value":11,"tag":"field"},{"name":"c_2","value":20,"tag":"field"},{"name":"c_3","value":21,"tag":"field"}]},
{"tag":"void","bitSize":null,"bitAlignment":null},
{"tag":"union","id":0,"name":"u_a","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:25:7","bitSize":32,"bitAlignment":32,"fields":[{"tag":"field","name":"pointer","type":{"tag":":pointer","type":{"tag":":void"}},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"value","type":{"tag":":int"},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null},{"tag":"field","name":"enumeration","type":{"tag":":enum","id":0,"name":"e_a"},"bitSize":32,"bitAlignment":32,"bitOffset":0,"bitfield":false,"bitWidth":null}]},
{"tag":"enum","id":4,"name":"","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:31:1","fields":[{"name":"anon_enum_0","value":0,"tag":"field"},{"name":"anon_enum_1","value":1,"tag":"field"}]},
{"tag":"long-long","bitSize":64,"bitAlignment":32},
{"tag":"typedef","name":"plain_type_t","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:44:19","type":{"tag":":long-long"}},
{"tag":"function","name":"plain_function","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:46:5","inline":false,"variadic":false,"returnType":{"tag":":int"},"storageClass":"none","parameters":[{"tag":"parameter","name":null,"type":{"tag":":pointer","type":{"tag":":int"}}},{"tag":"parameter","name":null,"type":{"tag":":enum","id":0,"name":"e_a"}}]},
{"tag":"function","name":"set_global_var","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:48:6","inline":false,"variadic":false,"returnType":{"tag":":void"},"storageClass":"none","parameters":[{"tag":"parameter","name":null,"type":{"tag":":int"}}]},
{"tag":"function","name":"sbv_function","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:50:5","inline":false,"variadic":false,"returnType":{"tag":":int"},"storageClass":"none","parameters":[{"tag":"parameter","name":"arg","type":{"tag":"s_b"}}]},
{"tag":"function","name":"sbv_return_function","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:52:11","inline":false,"variadic":false,"returnType":{"tag":":union","id":0,"name":"u_a"},"storageClass":"none","parameters":[{"tag":"parameter","name":"arg","type":{"tag":":struct","id":0,"name":"s_a"}},{"tag":"parameter","name":"name","type":{"tag":":array","size":16,"type":{"tag":":char"}}}]},
{"tag":":const","name":"RANDOM_CONSTANT","location":"\/home\/borodust\/devel\/repo\/bodge-projects\/claw\/src\/t\/c\/c.h:1:9","value":42}
]