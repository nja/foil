/*
 * Created on Dec 8, 2004
 *
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 */
package com.richhickey.foil;
import java.util.*;
/**
 * @author Rich
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class ReferenceManager implements IReferenceManager
    {
    Hashtable idToObj;
    Hashtable objToId;
    int nextId = 1;
    
    public ReferenceManager()
        {
        idToObj = new Hashtable();
        objToId = new Hashtable();
        }
    /* (non-Javadoc)
     * @see com.richhickey.foil.IReferenceManager#getIdForObject(java.lang.Object)
     */
    public int getIdForObject(Object o)
        {
        Object id = objToId.get(o);
        if(id == null)
            {
            id = new Integer(nextId++);
            idToObj.put(id,o);
            objToId.put(o,id);
            }
        return ((Integer)id).intValue();
        }

	public Object getObjectForId(int id) throws Exception
	    {
	    Object o = idToObj.get(new Integer(id));
	    if(o == null)
	        throw new Exception("Invalid reference id");
	    return o;
	    }

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReferenceManager#free(int)
     */
    public void free(int id) throws Exception
        {
        Object o = getObjectForId(id);
        objToId.remove(o);
        idToObj.remove(new Integer(id));
        }

    }
