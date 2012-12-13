(define-generic-mode pts-mode
  '()           ; comments 
  '("lambda"    ; keywords
    "Pi")
  nil           ; fontlock
  '(".lfoo\\'") ; file extensions
  nil           ; additional functions
  "Major mode for editing (literate) pts files.")

(provide 'pts-mode)