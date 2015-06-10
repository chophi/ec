/*
 * TCEmacs.java
 * Copyright (C) 2005 Tomasz Malesinski <tmal@mimuw.edu.pl>
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 *
 */

import javax.swing.*;
import java.awt.*;
import java.io.*;
 
import com.topcoder.client.contestant.ProblemComponentModel;
import com.topcoder.shared.problem.DataType;
import com.topcoder.shared.problem.Constraint;
import com.topcoder.shared.problem.TestCase;
import com.topcoder.shared.problem.Renderer;
import com.topcoder.shared.language.*;

public class TCEmacs {
  private JPanel panel;
  private JTextArea logTextArea;
  private String className;
  private Language language;

  public TCEmacs() {
             
    logTextArea = new JTextArea();
    logTextArea.setEditable(false);
             
    panel = new JPanel(new BorderLayout());
    panel.add(new JScrollPane(logTextArea), BorderLayout.CENTER);
             
  }
      
  protected void log(String message) {
    logTextArea.append(message + "\n");
  }

  public JPanel getEditorPanel() {
    // Return our panel
    return panel;
  }
      
  public String getSource() {
    // Return our source
    try {
      String source = GNUClient.elispEval("(topcoder-get-source "
					  + GNUClient.quote(className)
					  + " '" + languageId(language) + ")");
      log("got source: " + String.valueOf(source.length()) + " characters");
      return source;
    } catch (GNUClientException e) {
      log("get source failed: " + e.getMessage());
      return "";
    }
  }
      
  public void setSource(String source) {
    // Set the source
    try {
      String res = GNUClient.elispEval("(topcoder-set-source "
				       + GNUClient.quote(className)
				       + " '" + languageId(language) + " "
				       + GNUClient.quote(source) + ")");
      log("set source: " + (res.equals("ok\n") ? "ok" : "failed"));
    } catch (GNUClientException e) {
      log("set source failed: " + e.getMessage());
    }
  }

  public boolean isCacheable() {
    return false;
  }

  private String languageId(Language language) {
    switch (language.getId()) {
    case JavaLanguage.ID:
      return "java";
    case CPPLanguage.ID:
    default:
      return "cpp";
    case CSharpLanguage.ID:
      return "cs";
    case VBLanguage.ID:
      return "vb";
    }
  }

  private String typeInfo(DataType type, Language language) {
    return "(" + GNUClient.quote(type.getBaseName()) +
      " . " + String.valueOf(type.getDimension()) + ")";
  }

  public void setProblemComponent(ProblemComponentModel problem,
				  Language language,
				  Renderer renderer) {
    String form = "(topcoder-set-problem '(";

    this.language = language;
    this.className = problem.getClassName();

    form += "(language . " + languageId(language) + ") ";
    try {
      form += "(statement . " + GNUClient.quote(renderer.toHTML(language)) + ") ";
    } catch (Exception e) {
      log("Error during the problem statement rendering: " + e.getMessage());
    }
    form += "(class-name . " + GNUClient.quote(problem.getClassName()) + ") ";
    form += "(method-name . " + GNUClient.quote(problem.getMethodName())
      + ") ";

    form += "(params . (";
    DataType[] paramTypes = problem.getParamTypes();
    String[] paramNames = problem.getParamNames();
    for (int i = 0; i < paramTypes.length; i++)
      form += "(" + typeInfo(paramTypes[i], language)
	+ " . " + GNUClient.quote(paramNames[i]) + ") ";
    form += ")) ";

    form += "(return . "
      + typeInfo(problem.getReturnType(), language) + ") ";

    form += "(signature . "
      + GNUClient.quote(language.getMethodSignature(problem.getMethodName(),
						    problem.getReturnType(),
						    problem.getParamTypes(),
						    problem.getParamNames()))
      + ") ";

    form += "(test-cases . (";
    TestCase[] testCases =  problem.getTestCases();
    for (int i = 0; i < testCases.length; i++) {
      form += "((";
      String[] args = testCases[i].getInput();
      for (int j = 0; j < args.length; j++)
	form += GNUClient.quote(args[j]) + " ";
      form += ") . " + GNUClient.quote(testCases[i].getOutput()) + ") ";
    }
    form += ")) ";

    form += "))";
    try {
      String res = GNUClient.elispEval(form);
      log("open Emacs: " + (res.equals("ok\n") ? "ok" : "failed"));
    } catch (GNUClientException e) {
      log("open Emacs failed: " + e.getMessage());
    }
  }
}
