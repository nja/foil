using System;
using System.Reflection;
using System.Reflection.Emit;
using System.IO;
using System.Collections;

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for TypeBuilder.
	/// </summary>
	public class Implementor
	{

		static	public	String	checkForPropertyFuncs(String methodName)
		{
			if(methodName.StartsWith("get_"))
				return	methodName.Replace("get_","");
			if(methodName.StartsWith("set_"))
				return	methodName.Replace("set_","");
			return	methodName;
		}

		static	public	void	ImplementMethod(Type interfaceType,MethodInfo method,TypeBuilder typeBuilder,FieldInfo invoker)
		{
			// Don't need to do these...
			if(	method.Name=="Equals"
				||method.Name=="ToString"
				||method.Name=="Finalize")
				return;
			Type[]	args	=	Reflector.getParameterTypes(method.GetParameters());
			MethodBuilder methodBuilder 
				= typeBuilder.DefineMethod(		method.Name
											,	MethodAttributes.Public | MethodAttributes.Virtual
											,	method.ReturnType
											,	args);
			
			// Forward all calls to the invocationHandler delegate
			genDelegateCall(interfaceType,args,methodBuilder,invoker);
		}

		static	public	void	genDelegateCall(	Type			interfaceType
													,Type[]			parameters
													,MethodBuilder	methodBuilder
													,FieldInfo		invoker)
		{
			ILGenerator	IL	=	methodBuilder.GetILGenerator();
			
			IL.DeclareLocal(typeof(object[]));
			loadInt32(IL,parameters.Length);
			IL.Emit(OpCodes.Newarr, typeof(object));
			storeLocal(IL,0);

			for(int i = 0; i < parameters.Length; i++) 
			{
				loadLocal(IL,0);
				putArgInArray(IL,i, parameters[i]);
			}

			IL.DeclareLocal(typeof(MethodInfo));
			
			IL.Emit(OpCodes.Ldtoken,methodBuilder);
			IL.Emit(OpCodes.Call, typeof(MethodBase).GetMethod("GetMethodFromHandle"));
			storeLocal(IL,1);
			
			IL.Emit(OpCodes.Ldarg_0);
			IL.Emit(OpCodes.Ldfld, invoker);//	The delegate
			IL.Emit(OpCodes.Ldarg_0);		//	1st arg
			loadLocal(IL,1);				//	2nd arg array
			loadLocal(IL,0);				//	3rd arg array
			IL.Emit(OpCodes.Callvirt,(typeof(Proxy.InvocationHandler).GetMethod("Invoke")));
			emitReturnFromMethod(IL, methodBuilder.ReturnType);
			}

		// Helper functions for emitting MSIL
		static	private void emitReturnFromMethod(ILGenerator IL, Type returnType)
		{
			if (returnType == typeof(void)) 
				IL.Emit(OpCodes.Pop);
			else
			{
				if (returnType.IsValueType) 
					unbox(IL,returnType);
				else
					IL.Emit(OpCodes.Castclass, returnType);
			}
			IL.Emit(OpCodes.Ret);
		}

		static public void putArgInArray(ILGenerator IL,int index, Type arg)
		{
			loadInt32(IL,index);
			IL.Emit(OpCodes.Ldarg_S, index);

			if (arg.IsByRef)
			{
				arg = arg.GetElementType();
				loadRef(IL,arg);
			}

			if (arg.IsPrimitive || arg.IsValueType) 
				IL.Emit(OpCodes.Box);

			IL.Emit(OpCodes.Stelem_Ref);
		}

		static private void loadRef(ILGenerator IL,Type arg)
		{
			switch(Type.GetTypeCode(arg))
			{
				case TypeCode.Single: 
					IL.Emit(OpCodes.Ldind_R4); break;
				case TypeCode.Double: 
					IL.Emit(OpCodes.Ldind_R8); break;
				case TypeCode.SByte: 
					IL.Emit(OpCodes.Ldind_I1); break;				 
				case TypeCode.Int16: 
					IL.Emit(OpCodes.Ldind_I2); break;
				case TypeCode.Int32: 
					IL.Emit(OpCodes.Ldind_I4); break;
				case TypeCode.UInt64:
				case TypeCode.Int64: 
					IL.Emit(OpCodes.Ldind_I8); break;
				case TypeCode.Boolean:
				case TypeCode.Byte: 
					IL.Emit(OpCodes.Ldind_U1); break;
				case TypeCode.Char:
				case TypeCode.UInt16: 
					IL.Emit(OpCodes.Ldind_U2); break;
				case TypeCode.UInt32: 
					IL.Emit(OpCodes.Ldind_U4); break;
				default : 
					IL.Emit(OpCodes.Ldind_Ref); break;
			}
		}

		static	public void loadInt32(ILGenerator IL,Int32 v)
		{
			switch(v)
			{
				case 0:IL.Emit(OpCodes.Ldc_I4_0);
					break;
				case 1:IL.Emit(OpCodes.Ldc_I4_1);
					break;
				case 2:IL.Emit(OpCodes.Ldc_I4_2);
					break;
				case 3:IL.Emit(OpCodes.Ldc_I4_3);
					break;
				case 4:IL.Emit(OpCodes.Ldc_I4_4);
					break;
				case 5:IL.Emit(OpCodes.Ldc_I4_5);
					break;
				case 6:IL.Emit(OpCodes.Ldc_I4_6);
					break;
				case 7:IL.Emit(OpCodes.Ldc_I4_7);
					break;
				case 8:IL.Emit(OpCodes.Ldc_I4_8);
					break;
				default:IL.Emit(OpCodes.Ldc_I4_S, v);
					break;
			}
		}

		static	public void loadLocal(ILGenerator IL,int index)
		{
			switch(index)
			{
				case 0:IL.Emit(OpCodes.Ldloc_0);
					break;
				case 1:IL.Emit(OpCodes.Ldloc_1);
					break;
				case 2:IL.Emit(OpCodes.Ldloc_2);
					break;
				case 3:IL.Emit(OpCodes.Ldloc_3);
					break;
				default:IL.Emit(OpCodes.Ldloc_S, index);
					break;
			}
		}

		static	public void storeLocal(ILGenerator	IL,int index)
		{
			switch(index)
			{
				case 0:IL.Emit(OpCodes.Stloc_0);
					break;
				case 1:IL.Emit(OpCodes.Stloc_1);
					break;
				case 2:IL.Emit(OpCodes.Stloc_2);
					break;
				case 3:IL.Emit(OpCodes.Stloc_3);
					break;
				default:IL.Emit(OpCodes.Stloc_S, index);
					break;
			}
		}

		static	public	void	unbox(ILGenerator IL,Type type)
		{
			IL.Emit(OpCodes.Unbox, type);
			if (type.IsPrimitive) 
				if(type.IsEnum)
					IL.Emit(getPrimitiveBoxOpCode(Enum.GetUnderlyingType(type)));
				else
					IL.Emit(getPrimitiveBoxOpCode(type));
			else
				IL.Emit(OpCodes.Ldobj, type);
		}

		static public	OpCode	getPrimitiveBoxOpCode(Type type)
		{
			if(type==typeof(sbyte))  return  OpCodes.Ldind_I1;
			if(type==typeof(short))  return  OpCodes.Ldind_I2;
			if(type==typeof(int))    return  OpCodes.Ldind_I4;
			if(type==typeof(long))   return  OpCodes.Ldind_I8;
			if(type==typeof(byte))   return  OpCodes.Ldind_U1;
			if(type==typeof(ushort)) return  OpCodes.Ldind_U2;
			if(type==typeof(uint))   return  OpCodes.Ldind_U4;
			if(type==typeof(ulong))  return  OpCodes.Ldind_I8;
			if(type==typeof(float))  return  OpCodes.Ldind_R4;
			if(type==typeof(double)) return  OpCodes.Ldind_R8;
			if(type==typeof(char))   return  OpCodes.Ldind_U2;
			if(type==typeof(bool))   return  OpCodes.Ldind_I1;
			throw new ArgumentOutOfRangeException("no opcode for type {0}",type.ToString());
		}
	}
}
