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

/**
 * @author Rich
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public interface IReflector
    {
    /**
     * @author Rich
     *
     * TODO To change the template for this generated type comment go to
     * Window - Preferences - Java - Code Style - Code Templates
     */
    ICallable getCallable(int memberType, Class c, String memberName) throws Exception;
    }
