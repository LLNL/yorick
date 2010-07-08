

#if (defined(PROFILING_ON) || defined(TRACING_ON))
#define TAU_ENABLED
#endif

#ifdef TAU_ENABLED
#include <TAU.h>
#else

#define TAU_START(name)
#define TAU_STOP(name)

/* Otherwise, declare the macros as null. TAU API follows: */
#define TYPE_STRING(profileString, str)
#define PROFILED_BLOCK(name, type)

#define TAU_TYPE_STRING(profileString, str)
#define TAU_PROFILE(name, type, group)
#define TAU_PROFILE_TIMER(var, name, type, group)
#define TAU_PROFILE_START(var)
#define TAU_PROFILE_STOP(var)
#define TAU_PROFILE_STMT(stmt)
#define TAU_PROFILE_EXIT(msg)
#define TAU_PROFILE_INIT(argc, argv)
#define TAU_PROFILE_SET_NODE(node)
#define TAU_PROFILE_SET_CONTEXT(context)
#define TAU_PROFILE_SET_THREAD(thread)
#define TAU_PROFILE_GET_NODE() -1
#define TAU_PROFILE_GET_THREAD() -1
#define TAU_PROFILE_GET_CONTEXT() -1
#define TAU_PROFILE_SET_GROUP_NAME(newname)
#define TAU_PROFILE_TIMER_SET_GROUP_NAME(t, newname)
#define TAU_PROFILE_CALLSTACK()
#define TAU_DB_DUMP()
#define TAU_DB_PURGE()
#define TAU_DB_DUMP_PREFIX(prefix)
#define TAU_DB_DUMP_INCR()
#define TAU_GET_FUNC_NAMES(functionList, num)
#define TAU_DUMP_FUNC_NAMES()
#define TAU_GET_PROFILE_GROUP(group)
#define TAU_INIT(argc, argv)


#define TAU_REGISTER_CONTEXT_EVENT(event, name)
#define TAU_CONTEXT_EVENT(event, data)
#define TAU_DISABLE_CONTEXT_EVENT(event)
#define TAU_ENABLE_CONTEXT_EVENT(event)


#define TAU_REGISTER_EVENT(event, name)
#define TAU_EVENT(event, data)
#define TAU_EVENT_DISABLE_MIN(event)
#define TAU_EVENT_DISABLE_MAX(event)
#define TAU_EVENT_DISABLE_MEAN(event)
#define TAU_EVENT_DISABLE_STDDEV(event)
#define TAU_REPORT_STATISTICS()
#define TAU_REPORT_THREAD_STATISTICS()
#define TAU_REGISTER_THREAD()
#define TAU_REGISTER_FORK(id, op)
#define TAU_ENABLE_INSTRUMENTATION()
#define TAU_DISABLE_INSTRUMENTATION()
#define TAU_ENABLE_GROUP(group)
#define TAU_DISABLE_GROUP(group)
#define TAU_ENABLE_GROUP_NAME(group)
#define TAU_DISABLE_GROUP_NAME(group)
#define TAU_ENABLE_ALL_GROUPS()
#define TAU_DISABLE_ALL_GROUPS()
#define TAU_TRACK_MEMORY()
#define TAU_TRACK_MEMORY_HERE()
#define TAU_ENABLE_TRACKING_MEMORY()
#define TAU_DISABLE_TRACKING_MEMORY()
#define TAU_TRACK_MEMORY()
#define TAU_TRACK_MEMORY_HERE()
#define TAU_SET_INTERRUPT_INTERVAL(value)

#define CT(obj)

#define TAU_TRACE_SENDMSG(type, destination, length)
#define TAU_TRACE_RECVMSG(type, source, length)

#define TAU_MAPPING(stmt, group) stmt
#define TAU_MAPPING_OBJECT(FuncInfoVar)
#define TAU_MAPPING_LINK(FuncInfoVar, Group)
#define TAU_MAPPING_PROFILE(FuncInfoVar)
#define TAU_MAPPING_CREATE(name, type, key, groupname, tid)
#define TAU_MAPPING_PROFILE_TIMER(Timer, FuncInfoVar, tid)
#define TAU_MAPPING_TIMER_CREATE(t, name, type, gr, group_name)
#define TAU_MAPPING_PROFILE_START(Timer, tid)
#define TAU_MAPPING_PROFILE_STOP(tid)
#define TAU_MAPPING_PROFILE_EXIT(msg, tid)
#define TAU_MAPPING_DB_DUMP(tid)
#define TAU_MAPPING_DB_PURGE(tid)
#define TAU_MAPPING_PROFILE_SET_NODE(node, tid)
#define TAU_MAPPING_PROFILE_SET_GROUP_NAME(timer, name)
#define TAU_PROFILE_TIMER_SET_NAME(t, newname)
#define TAU_PROFILE_TIMER_SET_TYPE(t, newname)
#define TAU_PROFILE_TIMER_SET_GROUP(t, id)
#define TAU_MAPPING_PROFILE_SET_NAME(timer, name)
#define TAU_MAPPING_PROFILE_SET_TYPE(timer, name)
#define TAU_MAPPING_PROFILE_SET_GROUP(timer, id)
#define TAU_MAPPING_PROFILE_GET_GROUP_NAME(timer)
#define TAU_MAPPING_PROFILE_GET_GROUP(timer)
#define TAU_MAPPING_PROFILE_GET_NAME(timer)
#define TAU_MAPPING_PROFILE_GET_TYPE(timer)



#define TAU_PHASE(name, type, group)
#define TAU_PHASE_CREATE_STATIC(var, name, type, group)
#define TAU_PHASE_CREATE_DYNAMIC(var, name, type, group)
#define TAU_PHASE_START(var)
#define TAU_PHASE_STOP(var)
#define TAU_GLOBAL_PHASE(timer, name, type, group)
#define TAU_GLOBAL_PHASE_START(timer)
#define TAU_GLOBAL_PHASE_STOP(timer)
#define TAU_GLOBAL_PHASE_EXTERNAL(timer)
#define TAU_GLOBAL_TIMER(timer, name, type, group)
#define TAU_GLOBAL_TIMER_EXTERNAL(timer)
#define TAU_GLOBAL_TIMER_START(timer)
#define TAU_GLOBAL_TIMER_STOP()
#define TAU_PROFILE_PARAM1L(a,b)
#define TAU_NEW(expr, size)             expr
#define TAU_DELETE(expr, variable)      expr

#define TAU_PROFILE_SNAPSHOT(name)
#define TAU_PROFILE_SNAPSHOT_1L(name, expr)
#define TAU_METADATA(name, value)
#define TAU_PHASE_METADATA(name, value)
#define TAU_CONTEXT_METADATA(name, value)

/* extensions to the PHASE/TIMER API */
#define TAU_DYNAMIC_PHASE(name, type, group)
#define TAU_DYNAMIC_PROFILE(name, type, group)
#define TAU_STATIC_PHASE_START(name)
#define TAU_STATIC_PHASE_STOP(name)
#define TAU_DYNAMIC_PHASE_START(name)
#define TAU_DYNAMIC_PHASE_STOP(name)
#define TAU_DYNAMIC_TIMER_START(name)
#define TAU_DYNAMIC_TIMER_STOP(name)
#define TAU_PROFILE_CREATE_DYNAMIC_AUTO(var, name, type, group)
#define TAU_PHASE_CREATE_DYNAMIC_AUTO(var, name, type, group)
#define TAU_PROFILER_CREATE(handle, name, type, group)
#define TAU_PROFILER_START(handle)
#define TAU_PROFILER_STOP(handle)
#define TAU_PROFILER_GET_INCLUSIVE_VALUES(handle, data)
#define TAU_PROFILER_GET_EXCLUSIVE_VALUES(handle, data)
#define TAU_PROFILER_GET_CALLS(handle, number)
#define TAU_PROFILER_GET_CHILD_CALLS(handle, number)
#define TAU_PROFILER_GET_COUNTER_INFO(counters, numcounters)

#define TAU_PROFILE_TIMER_DYNAMIC(var,name, type, group)
#define TAU_GET_FUNC_NAMES(functionList, num)
#define TAU_GET_COUNTER_NAMES(counterList, num)

#define TAU_GET_FUNC_VALS(v1,v2,v3,v4,v5,v6,v7,v8)
#define TAU_DUMP_FUNC_VALS(functionList, num)
#define TAU_DUMP_FUNC_VALS_INCR(functionList, num)
#define TAU_GET_EVENT_NAMES(eventList, num)
#define TAU_GET_EVENT_VALS(v1,v2,v3,v4,v5,v6,v7)
#define TAU_EVENT_SET_NAME(event, name)

#define TAU_ENABLE_TRACKING_MEMORY()
#define TAU_DISABLE_TRACKING_MEMORY()
#define TAU_TRACK_MEMORY()
#define TAU_TRACK_MEMORY_HERE()
#define TAU_TRACK_MEMORY_HEADROOM()
#define TAU_TRACK_MEMORY_HEADROOM_HERE()
#define TAU_ENABLE_TRACKING_MEMORY_HEADROOM()
#define TAU_DISABLE_TRACKING_MEMORY_HEADROOM()


#define TAU_QUERY_DECLARE_EVENT(event)
#define TAU_QUERY_GET_CURRENT_EVENT(event)
#define TAU_QUERY_GET_EVENT_NAME(event, str)
#define TAU_QUERY_GET_PARENT_EVENT(event)


#define TAU_SET_USER_CLOCK(value)

#endif /* TAU_ENABLED */
