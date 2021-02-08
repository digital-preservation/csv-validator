/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java.exceptions;
/**
 * ResourceBundle Exception (failure)
 */
public class ResourceBundleException extends Throwable{

	private static final long serialVersionUID = 1L;
	/**
     * @param message The failure message
     */
	public ResourceBundleException(String message) {
		super(message);
	}
}
