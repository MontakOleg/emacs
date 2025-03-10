(defun upcase-first (str)
  "Convert the first letter of STR to uppercase and leave the rest unchanged."
  (if (zerop (length str))
      ""
    (concat (upcase (substring str 0 1)) 
            (substring str 1))))
