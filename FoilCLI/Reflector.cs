using System;
using System.Reflection;
using System.Collections;

namespace  com.richhickey.foil 
{
	public class Reflector :IReflector
	{
		public class CallableMethod :ICallable
		{
			ArrayList methods;

			public CallableMethod(ArrayList methods)
			{
				this.methods = methods;
			}
        
			public Object invoke(ArrayList args) 
			{
				Object target = args[0];
				args = args.GetRange(1,args.Count-1);
				foreach(MethodInfo m in methods)
				{
					ParameterInfo[] parameters = m.GetParameters();
					if(isCongruent(parameters,args))
					{
						Object[] boxedArgs = boxArgs(parameters,args);
						try
						{
							return m.Invoke(target,boxedArgs);
						}
						catch(Exception ex)
						{	
							throw new TargetInvocationException(ex);
						}
					}
				}
				throw new TargetInvocationException(new Exception("no matching function found"));
			}
		}

		static Object[] boxArgs(ParameterInfo[] parameters,ArrayList args)
		{
			if(parameters.Length == 0)
				return null;
			Object[] ret = new Object[parameters.Length];
			for(int i=0;i<parameters.Length;i++)
			{
				Object arg		= args[i];
				Type paramType	= parameters[i].ParameterType; 
				ret[i]			= boxArg(paramType,arg);
			}
			return ret;
		}
    
		static Object boxArg(Type paramType,Object arg)
		{
			if(paramType == Type.GetType("System.Boolean") && arg == null)
				return false;
			else
				return arg;
		}
    
		static Boolean isCongruent(ParameterInfo[] parameters,ArrayList args)
		{
			Boolean ret = false;
			if(parameters.Length == args.Count)
			{
				ret = true;
				for(int i=0;ret && i<parameters.Length;i++)
				{
					Object arg		= args[i];
					Type argType	= (arg == null)?null:arg.GetType();
					Type paramType	= parameters[i].ParameterType; 
					TypeCode	paramTypeCode	=	Convert.GetTypeCode(paramType);
					if(paramType == Type.GetType("System.Boolean"))
					{
						ret = arg == null || argType == Type.GetType("System.Boolean");
					}
					else if(paramType.IsPrimitive)
					{
						if(arg == null)
							ret = false;
						else 
							ret	=	paramType.Equals(argType);
					}
					else
					{
						ret = arg == null 
							|| argType == paramType 
							|| paramType.IsAssignableFrom(argType);
					}
				}
			}
			return ret;
		}
    
		public ICallable getCallable(int memberType, Type c, String memberName)
		{
			switch(memberType)
			{
				case ICallableFlags.METHOD:
					return getMethod(c,memberName);
				default:
					throw new Exception("unsupported member type");
			}
		}
    
    
		ICallable getMethod(Type c,String method)
		{
			MethodInfo[] allmethods = c.GetMethods();
			ArrayList methods = new ArrayList();
			for(int i=0;i<allmethods.Length;i++)
			{
				if(method	==	allmethods[i].Name)
					methods.Add(allmethods[i]);
			}
			if(methods.Count == 0)
				throw new Exception("no methods found");
			return new CallableMethod(methods);
		}
	}
}
												  
