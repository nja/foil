/*
 * Created on Dec 10, 2004
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

import java.io.IOException;
import java.io.Writer;

/**
 * @author Rich
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class UniversalMarshaller implements IMarshaller
    {

    /* (non-Javadoc)
     * @see com.richhickey.foil.IMarshaller#marshall(java.lang.Object, java.io.Writer, com.richhickey.foil.IBaseMarshaller, int, int)
     */
    public void marshall(Object o, Writer w, IBaseMarshaller baseMarshaller,
            int flags, int depth)  throws IOException
        {
        if(baseMarshaller.canMarshallAsList(o))
            baseMarshaller.marshallAsVector(o,w,flags,depth);
        else	//todo, use beaninfo to dump properties as alist
            baseMarshaller.marshallAtom(o.toString(),w,flags,depth);
        }

    }
