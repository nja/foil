using System;
using System.IO;
using System.Collections;
using System.Reflection;


namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for RuntimeServer.
	/// </summary>
	public class RuntimeServer:IRuntimeServer
	{
		    
    IReader				reader;
    IBaseMarshaller		marshaller;
    IReferenceManager	referenceManager;
    IReflector			reflector;

    public RuntimeServer(	IReader reader
							, IBaseMarshaller marshaller
							, IReferenceManager referenceManager
							, IReflector reflector
						)
        {
        this.reader				= reader;
        this.marshaller			= marshaller;
        this.referenceManager	= referenceManager;
        this.reflector			= reflector;
        }

    /*
     * (non-Javadoc)
     * 
     * @see com.richhickey.foil.IRuntimeServer#processMessages()
     */
public void processMessages(TextReader ins,TextWriter outs) 
	{
	for(;;)
		{
	    String resultMessage = null;
		try{
			ArrayList message = reader.readMessage(ins);
			if(isMessage(":call",message))
			    //(:call cref marshall-flags marshall-value-depth-limit args ...)
			    {
				ICallable c = (ICallable)message[1];
				int marshallFlags = intArg(message[2]);
				int marshallDepth = intArg(message[3]);
				Object ret = c.invoke(message.GetRange(4,message.Count-4));
				resultMessage = createRetString(ret,marshaller,marshallFlags,marshallDepth);
			    }
			else if(isMessage(":cref",message))
			    //(:cref member-type tref|"packageQualifiedTypeName" "memberName")
			    {
			    int memberType = intArg(message[1]);
			    Type c = typeArg(message[2]);
			    String memberName = stringArg(message[3]);
			    ICallable ret = reflector.getCallable(memberType,c,memberName);
				resultMessage = createRetString(ret,marshaller,IBaseMarshallerFlags.MARSHALL_ID,0);
			    }
			else if(isMessage(":tref",message))
			    //(:tref "packageQualifiedTypeName")
			    {
			    Type c = Type.GetType((String)message[1]);
				resultMessage = createRetString(c,marshaller,IBaseMarshallerFlags.MARSHALL_ID,1);
			    }
			else if(isMessage(":free",message))
			    //(:free refid ...)
			    {
			    for(int i=1;i<message.Count;i++)
			        {
			        int id = intArg(message[i]);
			        referenceManager.free(id);
			        }
				resultMessage = createRetString(null,marshaller,0,0);
			    }
			else if(isMessage(":str",message))
			    //(:str refid)
			    {
				resultMessage = createRetString(message[1].ToString(),marshaller,0,0);
			    }
			else if(isMessage(":equals",message))
			    //(:equals ref1 ref2)
			    {
			    Object o1 = message[1];
			    Object o2 = message[2];
			    Boolean ret = (o1 == null) ? (o2 == null) : o1.Equals(o2);
				resultMessage = createRetString(ret?true:false,marshaller,0,0);
			    }
			else if(isMessage(":type-of",message))
			    //(:type-of ref)
			    {
			    Type c = message[1].GetType();
				resultMessage = createRetString(c,marshaller,IBaseMarshallerFlags.MARSHALL_ID,1);
			    }
			else if(isMessage(":is-a",message))
			    //(:is-a ref tref|"packageQualifiedTypeName")
			    {
			    Object o = message[1];
			    Type c = typeArg(message[2]);
				resultMessage = createRetString(c.IsInstanceOfType(o)?true:false,marshaller,0,0);
			    }
			else if(isMessage(":hash",message))
			    //(:hash refid)
			    {
				resultMessage = createRetString(message[1].GetHashCode(),marshaller,0,0);
			    }
			}
		catch(Exception ex)
			{
		    if(ex is IOException)
		        throw (IOException)ex;
			else if(ex is TargetInvocationException )
		        {
			    TargetInvocationException  ite = (TargetInvocationException)ex;
			    ex = ite.InnerException;
		        }

		    outs.Write("(:err \"");
			outs.Write(ex.ToString());
			outs.Write("\" ");
			marshaller.marshallAsList(ex.StackTrace,outs,0,1);
			outs.Write(')');
			outs.Flush();
			}

		if(resultMessage != null)
		    {
		    outs.Write(resultMessage);
			outs.Flush();
		    }
		}
	}

	String stringArg(Object o)
	    {
	    return (String)o;
	    }
	
	int intArg(Object o)
	    {
	    return Convert.ToInt32(o);
	    }
	
	Type typeArg(Object arg) 
	    {
	    if(arg is Type)
	        return (Type)arg;
	    else if (arg is String)
	        {
	        return Type.GetType((String)arg);
	        }
	    else
	        throw new Exception("expecting type arg, either reference or packageQualifiedName string");
	    }
	
	Boolean isMessage(String type,ArrayList message)
	    {
	    return String.Compare(type,(String)message[0],true)==0;
	    }
	
	String createRetString(Object o,IBaseMarshaller marshaller,int flags,int depth) 
	    {
		StringWriter sw = new StringWriter();
		sw.Write("(:ret");
		marshaller.marshallAtom(o,sw,flags,depth);
		sw.Write(')');
		return sw.ToString();
	    }
	}
}
