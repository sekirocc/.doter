;; java-decompile.el --- Java class decompilation utilities
;;
;; Author: Your Name
;; Description: JADX-based Java decompilation for JDT virtual files
;;

;;; Code:

(defvar jdtls-jadx-cache-dir (expand-file-name "~/.emacs.d/.local/jadx-cache")
  "Directory to cache JADX decompilation results.")

(defun jdtls-jadx-decompile-async ()
  "Asynchronous JADX decompilation with caching to avoid UI blocking."
  (interactive)
  (when (and buffer-file-name
          (string-match-p "jdt:" buffer-file-name))
    (let* ((uri buffer-file-name)
            ;; Extract JAR path and class path
            (jar-path (expand-file-name "~/.m2/repository/com/xme/ai/llm-proxy-sdk/1.0.0/llm-proxy-sdk-1.0.0.jar"))
            (class-path (when (string-match "jdt:/contents/[^/]+/\\([^?]*\\)\\.class" uri)
                          (replace-regexp-in-string "/" "." (match-string 1 uri))))
            (cache-key (when (and jar-path class-path)
                         (md5 (concat jar-path ":" class-path))))
            (cache-file (when cache-key
                          (concat jdtls-jadx-cache-dir "/" cache-key ".java")))
            (target-buffer (current-buffer)))

      (if (not (and jar-path class-path (file-exists-p jar-path)))
        (message "Could not extract valid JAR/class info from JDT URI")

        (make-directory jdtls-jadx-cache-dir t)

        ;; Check cache first
        (if (file-exists-p cache-file)
          (jdtls-load-from-cache cache-file jar-path class-path)

          ;; Show loading message immediately
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "// Decompiling with JADX (first time, will be cached)...\n"))
            (insert (format "// JAR: %s\n" (file-name-nondirectory jar-path)))
            (insert (format "// Class: %s\n\n" class-path))
            (insert "// Please wait, JADX is running in background...\n")
            (insert "// You can continue using Emacs while this runs.\n"))

          ;; Start async decompilation
          (jdtls-start-jadx-process jar-path cache-file class-path target-buffer))))))

(defun jdtls-load-from-cache (cache-file jar-path class-path)
  "Load decompiled content from cache asynchronously."
  (message "Loading from cache...")
  (let ((target-buffer (current-buffer)))
    ;; Show loading message immediately
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "// Decompiled with JADX (cached) from: %s\n"
                (file-name-nondirectory jar-path)))
      (insert (format "// Class: %s\n\n" class-path))
      (insert "// Loading from cache...\n"))

    ;; Load content asynchronously
    (run-with-timer 0.01 nil
      (lambda ()
        (let ((java-source (with-temp-buffer
                             (insert-file-contents cache-file)
                             (buffer-string))))
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (delete-region (point-at-bol) (point))  ; Remove "Loading..." line
                (jdtls-insert-large-content java-source target-buffer)))))))))

(defun jdtls-start-jadx-process (jar-path cache-file class-path target-buffer)
  "Start JADX process asynchronously."
  (let ((temp-dir (make-temp-file "jadx-" t)))
    (message "Starting JADX decompilation...")

    ;; Use async process with minimal output
    (let ((proc (start-process "jadx-decompile" "*JADX-Output*"
                  "jadx" "-d" temp-dir "--quiet" jar-path)))
      (set-process-sentinel proc
        `(lambda (process event)
           (cond
             ((string= event "finished\n")
               (jdtls-handle-jadx-completion ,temp-dir ,cache-file ,class-path ,target-buffer))
             ((string-match-p "exited abnormally" event)
               (message "JADX decompilation failed")
               (delete-directory ,temp-dir t))))))))

(defun jdtls-handle-jadx-completion (temp-dir cache-file class-path target-buffer)
  "Handle JADX completion and update buffer."
  ;; Use timer to avoid blocking UI
  (run-with-timer 0.1 nil
    (lambda ()
      (jdtls-process-jadx-result temp-dir cache-file class-path target-buffer))))

(defun jdtls-process-jadx-result (temp-dir cache-file class-path target-buffer)
  "Process JADX result and update target buffer."
  (let* ((sources-dir (concat temp-dir "/sources"))
          (java-files (when (file-directory-p sources-dir)
                        (directory-files-recursively sources-dir "LLMCaller\\.java$"))))

    (if java-files
      (condition-case err
        (let ((java-file (car java-files)))
          ;; Save to cache
          (copy-file java-file cache-file)

          ;; Update target buffer first, then load content asynchronously
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (format "// Decompiled with JADX from: %s\n"
                          (file-name-nondirectory cache-file)))
                (insert (format "// Class: %s\n\n" class-path))
                (insert "// Loading content...\n")))

            ;; Load content asynchronously
            (run-with-timer 0.05 nil
              (lambda ()
                (let ((java-source (with-temp-buffer
                                     (insert-file-contents java-file)
                                     (buffer-string))))
                  (when (buffer-live-p target-buffer)
                    (with-current-buffer target-buffer
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (delete-region (point-at-bol) (point))  ; Remove "Loading..." line
                        (jdtls-insert-large-content java-source target-buffer))))))))))
      (error
        (message "Error processing JADX result: %s" err)))

    (message "JADX decompilation failed - no Java files found"))

  ;; Cleanup
  (delete-directory temp-dir t))


(defun jdtls-insert-large-content (content target-buffer)
  "Insert large content without blocking UI."
  (if (< (length content) 5000)
    ;; Small content, insert directly
    (progn
      (insert content)
      (jdtls-finalize-buffer target-buffer))

    ;; Large content, insert in very small chunks
    (let ((chunk-size 1000)  ; Smaller chunks
           (pos 0))
      (jdtls-insert-content-chunk content target-buffer pos chunk-size))))

(defun jdtls-insert-content-chunk (content target-buffer pos chunk-size)
  "Insert content chunk by chunk with progress indication."
  (let ((end-pos (min (+ pos chunk-size) (length content)))
         (progress (/ (* pos 100) (length content))))

    ;; Show progress
    (when (= (mod progress 10) 0)
      (message "Loading decompiled content... %d%%" progress))

    (insert (substring content pos end-pos))

    (if (< end-pos (length content))
      ;; More content to insert - use very short timer
      (run-with-timer 0.001 nil  ; Much shorter delay
        (lambda ()
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (jdtls-insert-content-chunk content target-buffer end-pos chunk-size)))))
      ;; Done inserting
      (jdtls-finalize-buffer target-buffer))))

(defun jdtls-finalize-buffer (target-buffer)
  "Finalize buffer setup after content insertion."
  (when (buffer-live-p target-buffer)
    (with-current-buffer target-buffer
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (java-ts-mode)
      (setq-local semantic-mode nil)

      ;; 延迟启动 eglot 以确保模式设置完成
      (run-with-timer 0.1 nil
        (lambda ()
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (jdtls-setup-eglot-for-decompiled-buffer)))))

      (message "JADX decompilation completed!"))))

(defun jdtls-setup-eglot-for-decompiled-buffer ()
  "Set up eglot for decompiled buffer with proper JDT URI."
  ;; 确保我们有原始的 JDT URI
  (when (and buffer-file-name
          (string-match-p "jdt:" buffer-file-name))

    ;; 确保 eglot 管理这个缓冲区
    (unless (eglot-current-server)
      (condition-case err
        (eglot-ensure)
        (error
          (message "Failed to start eglot for decompiled buffer: %s" err))))

    ;; 设置缓冲区为只读，因为这是反编译的代码
    (setq buffer-read-only t)

    (message "Decompiled buffer ready for LSP navigation")))

(defun disable-semantic-for-jdt-files ()
  "Disable semantic mode and other problematic features for JDT virtual files."
  (when (and buffer-file-name
          (string-match-p "jdt:" buffer-file-name))
    (setq-local semantic-mode nil)
    (setq-local global-semantic-mode nil)
    (setq-local auto-save-default nil)
    (setq-local make-backup-files nil)
    (setq-local create-lockfiles nil)

    ;; 确保反编译后的缓冲区能被 eglot 正确管理
    (when (and (> (buffer-size) 0)  ; 缓冲区不为空
            (not (eglot-current-server))) ; 没有 LSP 服务器
      (run-with-timer 0.5 nil
        (lambda ()
          (when (buffer-live-p (current-buffer))
            (with-current-buffer (current-buffer)
              (jdtls-setup-eglot-for-decompiled-buffer))))))))

;; Auto-decompile empty JDT buffers
(defun auto-decompile-jdt-buffer ()
  "Auto-decompile empty JDT buffers."
  (when (and buffer-file-name
          (string-match-p "jdt:" buffer-file-name)
          (= (buffer-size) 0))
    ;; Use timer to avoid blocking
    (run-with-timer 0.3 nil #'jdtls-jadx-decompile-async)))

;; Setup hooks
(add-hook 'java-ts-mode-hook 'disable-semantic-for-jdt-files)
(add-hook 'java-ts-mode-hook 'auto-decompile-jdt-buffer)

;; Provide keybinding
(with-eval-after-load 'java-ts-mode
  (define-key java-ts-mode-map (kbd "C-c d") #'jdtls-jadx-decompile-async))

;; 改进的 xref 处理，确保在反编译缓冲区中也能正确跳转
(defun jdtls-xref-find-definitions-in-decompiled-buffer ()
  "Enhanced xref-find-definitions that works in decompiled buffers."
  (interactive)
  (if (and buffer-file-name
        (string-match-p "jdt:" buffer-file-name)
        buffer-read-only)  ; 反编译的缓冲区是只读的
    ;; 在反编译缓冲区中，确保有 LSP 连接再跳转
    (progn
      (unless (eglot-current-server)
        (message "Setting up LSP connection...")
        (jdtls-setup-eglot-for-decompiled-buffer))
      ;; 使用增强的跳转处理
      (jdtls-smart-xref-find-definitions))
    ;; 普通缓冲区，正常跳转
    (call-interactively #'xref-find-definitions)))

(defun jdtls-smart-xref-find-definitions ()
  "Smart xref that tries to find the correct method in decompiled code."
  (let ((symbol (thing-at-point 'symbol))
        (original-point (point)))
    
    ;; 先尝试正常的 xref
    (condition-case err
        (progn
          (call-interactively #'xref-find-definitions)
          ;; 如果跳转成功但位置不准确，尝试智能定位
          (when (and symbol 
                     (string-match-p "jdt:" (or buffer-file-name "")))
            (jdtls-smart-locate-symbol symbol)))
      (error 
       (message "XRef failed: %s. Trying fallback method search..." err)
       ;; 如果 xref 失败，尝试文本搜索
       (jdtls-fallback-method-search symbol)))))

(defun jdtls-smart-locate-symbol (symbol)
  "Try to locate the symbol more accurately in decompiled code."
  (when symbol
    (save-excursion
      (goto-char (point-min))
      ;; 尝试多种匹配模式
      (let ((patterns (list
                       ;; 方法定义模式
                       (format "\\b%s\\s-*(" symbol)
                       ;; 字段定义模式  
                       (format "\\b\\w+\\s-+%s\\s-*[;=]" symbol)
                       ;; 类定义模式
                       (format "\\bclass\\s-+%s\\b" symbol)
                       ;; 普通符号模式
                       (format "\\b%s\\b" symbol)))
            found-pos)
        
        ;; 尝试每种模式
        (dolist (pattern patterns)
          (when (and (not found-pos) 
                     (re-search-forward pattern nil t))
            (setq found-pos (match-beginning 0))))
        
        ;; 如果找到更好的位置，跳转过去
        (when found-pos
          (goto-char found-pos)
          (recenter)
          (message "Located symbol: %s" symbol))))))

(defun jdtls-fallback-method-search (symbol)
  "Fallback method to search for symbols in current buffer."
  (when symbol
    (let ((case-fold-search nil))
      (if (search-forward symbol nil t)
          (progn
            (goto-char (match-beginning 0))
            (recenter)
            (message "Found symbol: %s (text search)" symbol))
        (message "Symbol not found: %s" symbol)))))

;; 重新绑定 M-. 以使用改进的跳转函数
(with-eval-after-load 'java-ts-mode
  (define-key java-ts-mode-map (kbd "M-.") #'jdtls-xref-find-definitions-in-decompiled-buffer))

(provide 'java-decompile)

;;; java-decompile.el ends here
