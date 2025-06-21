;;; cloutlu-core-helpers.el --- Core helper macros and functions -*- lexical-binding: t; -*-

;;;; Internal Helpers (Dependencies for the public macros)

(defmacro file! ()
  "Return the file of the file this macro was called."
  (or (bound-and-true-p byte-compile-current-file)
      load-file-name
      (buffer-file-name (buffer-base-buffer))
      (let ((file (car (last current-load-list))))
        (if (stringp file) file))
      (error "file!: cannot deduce the current file path")))

(defmacro dir! ()
  "Return the directory of the file in which this macro was called."
  (let (file-name-handler-alist)
    (file-name-directory (macroexpand '(file!)))))

(defun cloutlu--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (if (listp hooks) hooks (list hooks))))
    (if (eq (car-safe hooks) 'quote)
        (if (listp (cadr hooks)) (cadr hooks) (list (cadr hooks)))
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun cloutlu--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (cloutlu--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "cloutlu--setq-%s-for-%s-h"
                                          var mode))))))

(setplist 'cloutlu--fn-crawl '(%2 2 %3 3 %4 4 %5 5 %6 6 %7 7 %8 8 %9 9))
(defun cloutlu--fn-crawl (data args)
  (cond ((symbolp data)
         (when-let
             (pos (cond ((eq data '%*) 0)
                        ((memq data '(% %1)) 1)
                        ((get 'cloutlu--fn-crawl data))))
           (when (and (= pos 1)
                      (aref args 1)
                      (not (eq data (aref args 1))))
             (error "%% and %%1 are mutually exclusive"))
           (aset args pos data)))
        ((and (not (eq (car-safe data) 'fn!))
              (or (listp data)
                  (vectorp data)))
         (let ((len (length data))
               (i 0))
           (while (< i len)
             (cloutlu--fn-crawl (elt data i) args)
             (cl-incf i))))))


;;;; Public Macros

;;; Configuration & Loading Helpers

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE has loaded. A more powerful `with-eval-after-load'.
Supports compound package lists with :or and :and.
Does nothing if a package is disabled or not installed."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      `(with-eval-after-load ',package ,@body)
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defmacro load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name')."
  `(load
    (file-name-concat ,(or path `(dir!)) ,filename)
    ,noerror 'nomessage))

(defmacro add-load-path! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file."
  `(let ((default-directory (dir!))
         file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path :test #'string=))))


;;; Variable & Hook Management

(defmacro setq! (&rest settings)
  "A safer `setq` for setting customizable variables.
This triggers custom setters on variables defined with `defcustom`,
which `setq` does not. Use this instead of `setq` for package settings."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                              ',var ,val))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks."
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (cloutlu--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',(nreverse hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks."
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS."
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (cloutlu--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _) (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))


;;; Powerful Local Bindings

(defmacro lambda! (arglist &rest body)
  "Returns (cl-function (lambda ARGLIST BODY...))
The closure is wrapped in `cl-function', meaning ARGLIST will accept anything
`cl-defun' will. Implicitly adds `&allow-other-keys' if `&key' is present."
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
  `(cl-function
    (lambda
      ,(letf! (defun* allow-other-keys (args)
                (mapcar
                 (lambda (arg)
                   (cond ((not (listp (cdr-safe arg))) arg)
                         ((listp arg) (allow-other-keys arg))
                         (arg)))
                 (if (and (memq '&key args)
                          (not (memq '&allow-other-keys args)))
                     (if (memq '&aux args)
                         (let (newargs arg)
                           (while args
                             (setq arg (pop args))
                             (when (eq arg '&aux)
                               (push '&allow-other-keys newargs))
                             (push arg newargs))
                           (nreverse newargs))
                       (append args (list '&allow-other-keys)))
                   args)))
         (allow-other-keys arglist))
      ,@body)))

(put 'defun* 'lisp-indent-function 'defun)
(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.
A powerful combination of `cl-letf`, `cl-macrolet`, and temporary advice."
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice
               (if (keywordp (cadr rest))
                   (cl-destructuring-bind (target where fn) rest
                     `(when-let (fn ,fn)
                        (advice-add ,target ,where fn)
                        (unwind-protect ,body (advice-remove ,target fn))))
                 (let* ((fn (pop rest))
                        (argspec (pop rest)))
                   (when (< (length argspec) 3)
                     (setq argspec
                           (list (nth 0 argspec)
                                 (nth 1 argspec)
                                 (or (nth 2 argspec) (gensym (format "%s-a" (symbol-name fn)))))))
                   (let ((name (nth 2 argspec)))
                     `(progn
                        (define-advice ,fn ,argspec ,@rest)
                        (unwind-protect ,body
                          (advice-remove #',fn #',name)
                          ,(if name `(fmakunbound ',name))))))))
              (`defun
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  (cl-letf (((symbol-function #',(car rest))
                             (lambda! ,(cadr rest) ,@(cddr rest))))
                    ,body)))
              (`defun*
               `(cl-labels ((,@rest)) ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))


;;; Concise Lambda Helpers

(defmacro fn! (&rest args)
  "Return a lambda with implicit, positional arguments.
e.g. (fn! (message \"%s, %s\" %1 %2))"
  `(lambda ,(let ((argv (make-vector 10 nil)))
              (cloutlu--fn-crawl args argv)
              `(,@(let ((i (1- (length argv)))
                        (n -1)
                        sym arglist)
                    (while (> i 0)
                      (setq sym (aref argv i))
                      (unless (and (= n -1) (null sym))
                        (cl-incf n)
                        (push (or sym (intern (format "_%%%d" i)))
                              arglist))
                      (cl-decf i))
                    arglist)
                ,@(and (aref argv 0) '(&rest %*))))
     ,@args))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body).
A factory for quickly producing interactive commands."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional prefix-arg &rest args)
  "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))



;;; Concise Lambda Helpers (continue here)

(defmacro fn! (&rest args)
  "Return a lambda with implicit, positional arguments.
e.g. (fn! (message \"%s, %s\" %1 %2))"
  `(lambda ,(let ((argv (make-vector 10 nil)))
              (cloutlu--fn-crawl args argv)
              `(,@(let ((i (1- (length argv)))
                        (n -1)
                        sym arglist)
                    (while (> i 0)
                      (setq sym (aref argv i))
                      (unless (and (= n -1) (null sym))
                        (cl-incf n)
                        (push (or sym (intern (format "_%%%d" i)))
                              arglist))
                      (cl-decf i))
                    arglist)
                ,@(and (aref argv 0) '(&rest %*))))
     ,@args))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body).
A factory for quickly producing interactive commands."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional prefix-arg &rest args)
  "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))

;; --- ADD THE CMDS! MACRO HERE ---
(defmacro cmds! (&rest branches)
  "Returns a dispatcher that runs a command in BRANCHES.
Meant to be used as a target for keybinds (e.g. with `define-key' or `map!').

BRANCHES is a flat list of CONDITION COMMAND pairs. CONDITION is a lisp form
that is evaluated when (and each time) the dispatcher is invoked. If it returns
non-nil, COMMAND is invoked, otherwise it falls through to the next pair.

The last element of BRANCHES can be a COMMANd with no CONDITION. This acts as
the fallback if all other conditions fail.

Otherwise, Emacs will fall through the keybind and search the next keymap for a
keybind (as if this keybind never existed)."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    (let ((defs (cl-loop for (key value) on branches by 'cddr
                         unless (keywordp key)
                         collect (list key value))))
      `'(menu-item
         ,(or docstring "") nil
         :filter (lambda (&optional _)
                   (let (it)
                     (cond ,@(mapcar (lambda (pred-def)
                                       `((setq it ,(car pred-def))
                                         ,(cadr pred-def)))
                                     defs)
                           (t ,fallback))))))))

(provide 'cloutlu-core-helpers)
;;; cloutlu-core-helpers.el ends here
