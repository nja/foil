using System;

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for IReferenceManager.
	/// </summary>
	public interface IReferenceManager
	{
		int getIdForObject(Object o);
		Object getObjectForId(int id);
		void free(int id);
	}
}
