(s:eval-always
  (ql:quickload :and-jni)
  (ql:quickload :str)
  (nick #:j #:and-jni
        #:jll #:and-jni/cffi))

(cffi:load-foreign-library "/lib/jvm/java-20-openjdk-amd64/lib/server/libjvm.so")

(j:create-vm :options `((,(concatenate
                           'string
                           "-Djava.class.path="
                           (str:join ":"
                                     '( ;"/home/grolter/jars/fglib.jar"
                                       "/home/grolter/jars/fglib-w.jar"
                                        ;"/usr/share/java/javafx-base.jar"
                                        ;"/usr/share/java/javafx-graphics.jar"
                                        ;"/usr/share/java/javafx-controls.jar"
                                        ;"/usr/share/java/kotlin-stdlib.jar"
                                       ))))
                        (,(concatenate
                           'string
                           "--module-path="
                           (str:join ":"
                                     '("/home/grolter/jars/javafx-sdk-21/lib"
                                       "/usr/share/java/kotlin-stdlib.jar"
                                       "/home/grolter/jars/fglib-v0.8.8.1.jar"
                                       "/home/grolter/jars/ProjectAAA.jar"
                                       ))))
                        (,(concatenate
                           'string
                           "--add-modules="
                           (str:join ","
                                     '("javafx.base"
                                       "javafx.graphics"
                                        ;"javafx.controls"
                                        ;"javafx.fxml"
                                       "kotlin.stdlib"
                                       "FGLib_K"
                                       "ProjectAAA"
                                       ))))
                        ("-Xrs")))

(defparameter *clock* nil)

(j:define-native-method "gleefre/wrapper/fglib/LispExtension" "init" :void ()
  (setf *clock* (sc:make-clock))
  (format t "Yay, we're initializing our game~%"))

(j:define-native-method "gleefre/wrapper/fglib/LispExtension" "update" :void ()
  (format t "Game is running: ~A~%" *clock*))


(defmacro je (&body body &aux (env (gensym)) (e (gensym)))
  `(block nil
     (j:with-env (,env)
       (unwind-protect (progn ,@body)
         (let ((,e (jll:exception-occurred ,env)))
           (unless (cffi:null-pointer-p ,e)
             (return
               (multiple-value-prog1 (values (j:jstring-to-string
                                              (j:jcall :string ("java/lang/Class" "getCanonicalName")
                                                       (jll:get-object-class ,env ,e)))
                                             (j:jstring-to-string
                                              (j:jcall :string ("java/lang/Throwable" "toString") ,e))
                                             (j:jstring-to-string
                                              (j:jcall :string ("java/lang/Throwable" "getMessage") ,e)))
                 (jll:exception-clear ,env)))))))))

(je
  (j:jcall :void ("com/uzery/fglib/core/program/Launcher" "startProcess")
           (j:jclass "com/uzery/fglib/core/program/Launcher")
           (:array (:class "com/uzery/fglib/core/program/Extension"))
           (j:jarray "gleefre/wrapper/fglib/LispExtension"
                     (j:jnew "gleefre/wrapper/fglib/LispExtension"))))

#|

// LispExtension.java

package gleefre.wrapper.fglib;

import com.uzery.fglib.core.program.Extension;
import java.util.List;

public class LispExtension implements Extension {
  @Override public native void init();
  @Override public native void update();
  @Override public native List<Extension> children();
  @Override public native boolean isRunning();
}

|#
