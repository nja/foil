using System;

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for IReferenceManager.
	/// </summary>
	public interface IReferenceManager
	{
		public	int getIdForObject(Object o);
		public	Object getObjectForId(int id);
		public	void free(int id;
	}
}
