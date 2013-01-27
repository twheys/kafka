
-record(module, {
name,emod,actions=[]
}).
-record(action, {
name,efun,arity,roles
}).