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
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class RuntimeServer implements IRuntimeServer
    {

    
    IReader reader;

    IBaseMarshaller marshaller;

    IReferenceManager referenceManager;

    public RuntimeServer(IReader reader, IBaseMarshaller marshaller,
            IReferenceManager referenceManager)
        {
        this.reader = reader;
        this.marshaller = marshaller;
        this.referenceManager = referenceManager;
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
				int marshallFlags = Integer.parseInt((String)message.get(2));
				int marshallDepth = Integer.parseInt((String)message.get(3));
				Object ret = c.invoke(message.subList(4,message.size()));
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
			    }
			else if(isMessage(":tref",message))
			    //(:tref "packageQualifiedTypeName")
			    {
			    Class c = Class.forName((String)message.get(1));
				resultMessage = createRetString(c,marshaller,IBaseMarshaller.MARSHALL_ID,1);
			    }
			}
		catch(Exception ex)
			{
		    String message;
		    String trace;
		    if(ex instanceof IOException)
		        throw (IOException)ex;
			else if(ex instanceof InvocationTargetException)
		        {
			    InvocationTargetException ite = (InvocationTargetException)ex;
			    message = ite.getTargetException().getMessage();
			    trace = ite.getTargetException().getStackTrace().toString();
		        }
			else
			    {
			    message = ex.getMessage();
			    trace = ex.getStackTrace().toString();
			    }
			outs.write("(:err ");
			outs.write(message);
			outs.write(' ');
			outs.write(trace);
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

	boolean isMessage(String type,List message)
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
        }
    }