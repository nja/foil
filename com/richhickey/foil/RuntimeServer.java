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
 * 
 * 
 */
package com.richhickey.foil;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.util.List;


/**
 * @author Rich
 * 
 */
public class RuntimeServer implements IRuntimeServer
    {

    
    IReader reader;

    IBaseMarshaller marshaller;

    IReferenceManager referenceManager;
    
    IReflector reflector;

    public RuntimeServer(IReader reader, IBaseMarshaller marshaller,
            IReferenceManager referenceManager, IReflector reflector)
        {
        this.reader = reader;
        this.marshaller = marshaller;
        this.referenceManager = referenceManager;
        this.reflector = reflector;
        }

    /*
     * (non-Javadoc)
     * 
     * @see com.richhickey.foil.IRuntimeServer#processMessages()
     */
public void processMessages(Reader ins,Writer outs) throws IOException{
	for(;;)
		{
	    String resultMessage = null;
		try{
			List message = reader.readMessage(ins);
			if(isMessage(":call",message))
			    //(:call cref marshall-flags marshall-value-depth-limit args ...)
			    {
				ICallable c = (ICallable)message.get(1);
				int marshallFlags = intArg(message.get(2));
				int marshallDepth = intArg(message.get(3));
				Object ret = c.invoke(message.get(4),message.subList(5,message.size()));
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
			    }
			else if(isMessage(":cref",message))
			    //(:cref member-type tref|"packageQualifiedTypeName" "memberName")
			    {
			    int memberType = intArg(message.get(1));
			    Class c = typeArg(message.get(2));
			    String memberName = stringArg(message.get(3));
			    ICallable ret = reflector.getCallable(memberType,c,memberName);
				resultMessage = createRetString(ret,marshaller,IBaseMarshaller.MARSHALL_ID,0);
			    }
			else if(isMessage(":new",message))
				//(:new tref marshall-flags marshall-value-depth-limit (args ...) property-inits ...)
			    {
			    Class c = typeArg(message.get(1));
				int marshallFlags = intArg(message.get(2));
				int marshallDepth = intArg(message.get(3));
				List args = (List)message.get(4);
			    Object ret = reflector.createNew(c,args);
			    //set props
			    if(message.size()>5)
			        {
			        reflector.setProps(ret,message.subList(5,message.size()));
			        }
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
			    }
			else if(isMessage(":tref",message))
			    //(:tref "packageQualifiedTypeName")
			    {
			    Class c = Class.forName((String)message.get(1));
				resultMessage = createRetString(c,marshaller,IBaseMarshaller.MARSHALL_ID,1);
			    }
			else if(isMessage(":free",message))
			    //(:free refid ...)
			    {
			    for(int i=1;i<message.size();i++)
			        {
			        int id = intArg(message.get(i));
			        referenceManager.free(id);
			        }
				resultMessage = createRetString(null,marshaller,0,0);
			    }
			else if(isMessage(":str",message))
			    //(:str refid)
			    {
				resultMessage = createRetString(message.get(1).toString(),marshaller,0,0);
			    }
			else if(isMessage(":equals",message))
			    //(:equals ref1 ref2)
			    {
			    Object o1 = message.get(1);
			    Object o2 = message.get(2);
			    boolean ret = (o1 == null) ? (o2 == null) : o1.equals(o2);
				resultMessage = createRetString(ret?Boolean.TRUE:Boolean.FALSE,marshaller,0,0);
			    }
			else if(isMessage(":vector",message))
			    {
			    //(:vector tref|"packageQualifiedTypeName" length value ...)			    {
			    Class c = typeArg(message.get(1));
				int length = intArg(message.get(2));
				Object ret = reflector.createVector(c,length,
    					message.subList(3,message.size()));
				resultMessage = createRetString(ret,marshaller,IBaseMarshaller.MARSHALL_ID,0);
			    }
			else if(isMessage(":vget",message))
			    //(:vget aref marshall-flags marshall-value-depth-limit index)
			    {
				int marshallFlags = intArg(message.get(2));
				int marshallDepth = intArg(message.get(3));
				int index = intArg(message.get(4));
				Object ret = reflector.vectorGet(message.get(1),index);
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
			    }
			else if(isMessage(":vset",message))
			    //(:vset aref index value)
			    {
				int index = intArg(message.get(2));
				reflector.vectorSet(message.get(1),index,message.get(3));
				resultMessage = createRetString(null,marshaller,0,0);
			    }
			else if(isMessage(":vlen",message))
			    //(:vlen aref)
			    {
				Object ret = reflector.vectorLength(message.get(1));
				resultMessage = createRetString(ret,marshaller,0,0);
			    }
			else if(isMessage(":bases",message))
			    //(:bases tref|"packageQualifiedTypeName")
			    {
			    Class c = typeArg(message.get(1));
				StringWriter sw = new StringWriter();
				sw.write("(:ret");
				marshaller.marshallAsList(reflector.bases(c),sw,IBaseMarshaller.MARSHALL_ID,1);
				sw.write(')');
				resultMessage = sw.toString(); 
			    }
			else if(isMessage(":type-of",message))
			    //(:type-of ref)
			    {
			    Class c = message.get(1).getClass();
				resultMessage = createRetString(c,marshaller,IBaseMarshaller.MARSHALL_ID,1);
			    }
			else if(isMessage(":is-a",message))
			    //(:is-a ref tref|"packageQualifiedTypeName")
			    {
			    Object o = message.get(1);
			    Class c = typeArg(message.get(2));
				resultMessage = createRetString(c.isInstance(o)?Boolean.TRUE:Boolean.FALSE,marshaller,0,0);
			    }
			else if(isMessage(":hash",message))
			    //(:hash refid)
			    {
				resultMessage = createRetString(new Integer(message.get(1).hashCode()),marshaller,0,0);
			    }
			else if(isMessage(":members",message))
			    //(:members :tref|"packageQualifiedTypeName")
			    {
			    Class c = typeArg(message.get(1));
				StringWriter sw = new StringWriter();
				sw.write("(:ret");
				reflector.members(c,sw);
				sw.write(')');
				resultMessage = sw.toString(); 
			    }
			else if(isMessage(":marshall",message))
			    //(:marshall ref marshall-flags marshall-value-depth-limit)
			    {
			    Object ret = message.get(1);
				int marshallFlags = intArg(message.get(2));
				int marshallDepth = intArg(message.get(3));
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
//			    IMarshaller m = marshaller.findMarshallerFor(ret.getClass());
//				StringWriter sw = new StringWriter();
//				sw.write("(:ret");
//				m.marshall(ret,sw,marshaller,marshallFlags,marshallDepth);
//				sw.write(')');
//				resultMessage = sw.toString(); 
			    }
			else
			    {
			    throw new Exception("unsupported message");
			    }
			}
		catch(Throwable ex)
			{
		    String message;
		    String trace;
		    if(ex instanceof IOException)
		        throw (IOException)ex;
		    
			while(ex.getCause() != null)
		        {
			    ex = ex.getCause();
		        }

		    outs.write("(:err");
			marshaller.marshallAtom(ex.toString(),outs,0,0);
			StringWriter sw = new StringWriter();
			//marshaller.marshallAsList(ex.getStackTrace(),outs,0,1);
			ex.printStackTrace(new PrintWriter(sw));
			marshaller.marshallAtom(sw.toString(),outs,0,0);
			outs.write(')');
			outs.flush();
			}

		if(resultMessage != null)
		    {
		    outs.write(resultMessage);
			outs.flush();
		    }
		}
	}

	String stringArg(Object o)
	    {
	    return (String)o;
	    }
	
	int intArg(Object o)
	    {
	    return ((Number)o).intValue();
	    }
	
	static Class typeArg(Object arg) throws Exception
	    {
	    if(arg instanceof Class)
	        return (Class)arg;
	    else if (arg instanceof String)
	        {
	        String tname = (String)arg;
	        if(tname.equalsIgnoreCase(":int"))
	            return int.class;
	        else if(tname.equalsIgnoreCase(":double"))
	            return double.class;
	        else if(tname.equalsIgnoreCase(":long"))
	            return long.class;
	        else if(tname.equalsIgnoreCase(":float"))
	            return float.class;
	        else if(tname.equalsIgnoreCase(":char"))
	            return char.class;
	        else if(tname.equalsIgnoreCase(":boolean"))
	            return boolean.class;
	        else if(tname.equalsIgnoreCase(":short"))
	            return short.class;
	        else if(tname.equalsIgnoreCase(":byte"))
	            return byte.class;
	        else
	            return Class.forName(tname);
	        }
	    else
	        throw new Exception("expecting type arg, either reference or packageQualifiedName string");
	    }
	
	static boolean isMessage(String type,List message)
	    {
	    return type.equalsIgnoreCase((String)message.get(0));
	    }
	
	String createRetString(Object o,IBaseMarshaller marshaller,int flags,int depth) throws IOException
	    {
		StringWriter sw = new StringWriter();
		sw.write("(:ret");
		marshaller.marshallAtom(o,sw,flags,depth);
		sw.write(')');
		return sw.toString();
	    }

	public static void main(String[] args)
        {
	    IReferenceManager referenceManager = new ReferenceManager();
	    BaseMarshaller baseMarshaller = new BaseMarshaller(referenceManager);
	    baseMarshaller.registerMarshaller(Object.class, new UniversalMarshaller());
	    IReflector reflector = new Reflector(baseMarshaller);
	    IReader reader = new MessageReader(referenceManager,reflector);
	    IRuntimeServer server = new RuntimeServer(reader,baseMarshaller,referenceManager,reflector);
	    try{
	        server.processMessages(new BufferedReader(new InputStreamReader(System.in)),
	            new BufferedWriter(new OutputStreamWriter(System.out)));
	    	}
        catch(Exception ex)
	    	{
	        System.out.println(ex.getMessage());
	    	}
        }
    }