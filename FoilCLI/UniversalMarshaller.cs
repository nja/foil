using System;
using System.IO;

	/**
	 * @author Eric Thorsen
	 *
	 * TODO To change the template for this generated type comment go to
	 * Window - Preferences - Java - Code Style - Code Templates
	 */
namespace com.richhickey.foil	
{
	public class UniversalMarshaller :IMarshaller
	{
		public void marshall(Object o, TextWriter w, IBaseMarshaller baseMarshaller,int flags, int depth)  
		{
			if(baseMarshaller.canMarshallAsList(o))
				baseMarshaller.marshallAsVector(o,w,flags,depth);
			else if(o is Type)
				baseMarshaller.marshallAtom(((Type)o).Name,w,flags,depth);
			else	//todo, use beaninfo to dump properties as alist
				baseMarshaller.marshallAtom(o.ToString(),w,flags,depth);
		}
	}
}
