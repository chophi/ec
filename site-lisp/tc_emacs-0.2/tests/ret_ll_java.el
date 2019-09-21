(topcoder-set-problem
 '((language . java)
   (statement . "return the sum of the a array elements
<pre>
    long r = 0;
    for (int i = 0; i < a.length; i++)
      r += a[i];
    return r;
<pre>
")
   (class-name . "Test1")
   (method-name . "testMethod")
   (params (("long" . 1) . "a")
	   (("String" . 1) . "b")
	   (("double" . 1) . "c")
	   (("int" . 1) . "d")
	   (("long" . 0) . "a1")
	   (("String" . 0) . "b1")
	   (("double" . 0) . "c1")
	   (("int" . 0) . "d1"))
   (return "long" . 0)
   (signature . "long testMethod(long[] a, String[] b, double[] c, int[] d, long a1, String b1, double c1, int d1)")
   (test-cases (("{1,2,3,4}" "{\"asdf\",\"qwer\"}" "{0.5,0.1}" "{0,1}"
		 "12345678901234" "\"zxcv\"" "1.234" "123")
		. "10")
	       (("{11111111111,22222222222}" "{\"asdf\"}" "{0.5,0.3}"
		 "{0,1}"
		 "12345678901234" "\"zxcv\"" "1.234" "123")
		. "33333333333"))))
