/*
 * Created on Dec 7, 2004
 *
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 */
using System;
using System.Collections;
using System.Reflection;
using System.Reflection.Emit;
//using System.Math;
/**
 * @author Eric Thorsen
 *
 */
namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for Proxy.
	/// </summary>
	public class Proxy
	{
		static	Random	rand	=	new Random((int)DateTime.Now.Ticks);

		public	delegate Object		InvocationHandler(		Object			proxy
														,	MethodInfo		method
														,	Object[]		args);

		static	public Object BuildProxy(InvocationHandler invocationHandler,params Type[]	interfaces)
		{
			if(interfaces.Length==0)
				throw new ArgumentOutOfRangeException("interfaces must contain at least 1 interface type");
			return	createInstance(invocationHandler,recurseInterfaces(interfaces));
		}

		static	internal	Type[]	recurseInterfaces(Type[] interfaces)
		{
			Hashtable	ht	=	new Hashtable();
			foreach(Type t in interfaces)
				doRecurseInterfaces(ht,t);
			Type[]	retval	=	new Type[ht.Count];
			ArrayList ar	=	new ArrayList();
			foreach(Type t in ht.Values)
				ar.Add(t);
			ar.CopyTo(retval);
			return	retval;
		}

		static	internal	void	doRecurseInterfaces(Hashtable	list,Type intface)
		{
			list.Add(intface.ToString(),intface);
			if(intface.BaseType==null||intface.BaseType==typeof(Object))
				return;
			doRecurseInterfaces(list,intface.BaseType);
		}
 
		static internal	Object	createInstance(InvocationHandler invoker,Type[] interfaces)
		{
			AppDomain appDomain = AppDomain.CurrentDomain;
			AssemblyName assemblyName = new AssemblyName();
			assemblyName.Name	= "_Dymamic__PROXY_TT_";
			AssemblyBuilder assemblyBuilder = appDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run);
			ModuleBuilder	moduleBuilder	= assemblyBuilder.DefineDynamicModule("_TT_DymanicModule_");
			// Need to do a better job coming up with unique names
			TypeBuilder builder	= moduleBuilder.DefineType(		String.Format("TTProxy{0}.{1}",rand.Next(),interfaces[0])
															,	TypeAttributes.Public
															,	null
															,	interfaces
															);
			// I would have thought that the derivation would have taken care of this for me?
			FieldBuilder	fb	=	builder.DefineField("invocationHandler",typeof(Proxy.InvocationHandler),FieldAttributes.Public);
			foreach(Type t in interfaces)
			{
				foreach(MethodInfo m in t.GetMethods(BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public))
					Implementor.ImplementMethod(t,m,builder,fb);
			}

			Type	newType			=	builder.CreateType();
			Object	proxy			=	Activator.CreateInstance(newType,true);
			FieldInfo[]	flds		=	newType.GetFields(BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public);
			FieldInfo invokerFld	=	newType.GetField("invocationHandler",BindingFlags.Instance|BindingFlags.NonPublic|BindingFlags.Public);
			invokerFld.SetValue(proxy, invoker);
			return	proxy;
		}
	}
}
