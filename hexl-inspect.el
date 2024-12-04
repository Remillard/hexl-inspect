;;; hexl-inspect.el --- A data inspection minor mode for hexl-mode -*- lexical-binding: t -*-
;;
;; Author: Mark Norton <remillard@gmail.com>
;; Keywords: hexl, data
;;
;;; Commentary:
;;
;; This package implements a minor mode named `hexl-inspect-mode' to be used in
;; conjunction with a buffer set to `hexl-mode'.  When activated, the minor mode
;; will create a data inspection buffer and window and display to the side of
;; the `hexl-mode' buffer.  As the point moves around in the parent buffer, the
;; contents will update to reflect the data at that position.
;;
;; The mode depends on the variable state of `hexl-inspect--big-endian-p'
;; which determines how the data is interpreted.
;;
;; The automated update structure and mode was patterened after the explore
;; mode in `treesit-explore-mode' in `treesit.el'.
;; 
;;; TODO 
;;
;; * Validation, foolproofing
;;   

;; Variables for inspection behavior set up per parent buffer.
(defvar-local hexl-inspect--big-endian-p nil
  "The boolean variable HEXL-INSPECT--BIG-ENDIAN-P is used to set
the endianness for hexl-inspect.")
(defvar-local hexl-inspect--endian-str nil
  "The string variable HEXL-INSPECT--ENDIAN-STR is used for the
hexl-inspect display.")
(defvar-local hexl-inspect--data-buf nil
  "HEXL-INSPECT--DATA-BUF defines the buffer used to display the
data inspection results.")
(defvar-local hexl-inspect--inspection-refresh-timer nil
  "Timer for refreshing the display buffer.")

(defvar hexl-inspect-mode)


;; This function toggles the state of the endianness
(defun hexl-inspect-toggle-endian ()
  "Alters the contents of the endianness variables."
  (interactive)
  (if hexl-inspect--big-endian-p
      (progn
        (setq-local hexl-inspect--big-endian-p nil)
        (setq-local hexl-inspect--endian-str " Little endian "))
    (progn
      (setq-local hexl-inspect--big-endian-p t)
      (setq-local hexl-inspect--endian-str "  Big endian   "))))

;; This function returns a 16 character string which is the most data I would
;; want to deconstruct for inspection at any one time.  Intended to be called in
;; the hexl-mode buffer.
(defun hexl-inspect--get-hex-str (big-endian-p)
  "Returns a hexadecimal string of 8 bytes with specified endian-ness
in the boolean BIG-ENDIAN-P.  BIG-ENDIAN-P truth will result in a
big-endian interpretation.  BIG-ENDIAN-P nil will result in a
little-endian interpretation."
  (save-excursion
    ;; Predefining small string to fill in rather than continue creating new
    ;; strings with the acquisition of every concatenation (prior method).
    (let ((word-str (make-string 16 ?0))
          (byte-str (make-string 2 ?0))
          (byte-ptr 0))
      ;; Issuing a zero forward character to make sure the point moves
      ;; to the beginning of a byte at the current address.
      (hexl-forward-char 0)
      (dotimes (ptr-idx 8)
        (setq byte-str (buffer-substring-no-properties (point) (+ 2 (point))))
        (if big-endian-p
            (setq byte-ptr (* 2 ptr-idx))
          (setq byte-ptr (- 16 (* 2 (+ 1 ptr-idx)))))
        (store-substring word-str byte-ptr byte-str)
        ;; Using hexl-forward-char from hexl.el as it advances in exactly
        ;; the way I want, skipping all the other text hexl produces.
        (hexl-forward-char 1))
      word-str)))

;; This function is used to subdivide a 16 character string of hex characters
;; based on the number of nibbles desired and the endianness the string
;; represents.
(defun hexl-inspect--word-str (64bit-word-str nibbles big-endian-p)
  "Based on the boolean BIG-ENDIAN-P defining the current endianness,
return a string of the number of NIBBLES from 64BIT-WORD-STR."
  (if big-endian-p
      (substring 64bit-word-str 0 nibbles)
    (substring 64bit-word-str (- 16 nibbles) nil)))

;; Given an unsigned integer value at a particular width, return the signed
;; two's complement value of that value.
(defun hexl-inspect--twos-complement (number bit-width)
  "Return the two's complement interpretation of NUMBER with given
BIT-WIDTH, if applicable."
  (let* ((max-value (expt 2 bit-width))
         (half-max (/ max-value 2)))
    (if (>= number half-max)
        (- number max-value)
      number)))

;; Converting a nibble character to a binary string is a helper function to
;; hexl-hex-str-to-bin.
(defun hexl-inspect--nibble-str-to-bin (nib-char)
  "Return a string containing the binary representation of the hex
nibbled NIB-CHAR."
  (pcase nib-char
    ('?0 "0000")
    ('?1 "0001")
    ('?2 "0010")
    ('?3 "0011")
    ('?4 "0100")
    ('?5 "0101")
    ('?6 "0110")
    ('?7 "0111")
    ('?8 "1000")
    ('?9 "1001")
    ((or '?a '?A) "1010")
    ((or '?b '?B) "1011")
    ((or '?c '?C) "1100")
    ((or '?d '?D) "1101")
    ((or '?e '?E) "1110")
    ((or '?f '?F) "1111")
    (_ "-NaN")))

;; Receives a string of some arbitrary length comprised of hex characters and
;; returns a string that contains the binary representation of the hex value.
(defun hexl-inspect--hex-str-to-bin (hex-str)
  "Return a string containing the binary representation of the
hexadecimal string HEX-STR."
  (let* ((bin-str-len (* 4 (length hex-str)))
         ;; Predefining string length in order to avoid multiple string creations.
         (bin-str (make-string bin-str-len ?0)))
    (dotimes (idx (length hex-str))
      (store-substring bin-str (* 4 idx) (hexl-inspect--nibble-str-to-bin (aref hex-str idx))))
    bin-str))

;; This function used for the character decode of the string.  Rewritten
;; to avoid former byte-compilation warning about an unused lexical binding
;; in the `dotimes' function.  Not entirely sure why it thought it wasn't
;; used, but this dodges that warning.
(defun hexl-inspect--decode-hex-string (hex-string)
  "Return a string of characters corresponding to the hex bytes in
HEX-STRING.  For example, hex string `43484950' would return
`CHIP'.  Will assume an even number of characters in the string,
and if odd will fail to address the odd ending character.
Unprintable characters will be returned as Elisp character
formats."
  (let* ((num-chars (/ (length hex-string) 2))
         (result-str (make-string num-chars ?.)))
    (dotimes (i num-chars)
      (let* ((byte-str (substring hex-string (* 2 i) (* 2 (+ 1 i))))
             (byte-char (format "%c" (string-to-number byte-str 16))))
        (store-substring result-str i byte-char)))
    result-str))

;; This is the ongoing buffer refresh version of the original inspection
;; command.
(defun hexl-inspect--inspection-refresh ()
  "Updates the hexl inspection buffer."
  (when (and hexl-inspect-mode
             (buffer-live-p hexl-inspect--data-buf))
    (let* ((64bit-word-str (hexl-inspect--get-hex-str hexl-inspect--big-endian-p))
           (64bit-unsigned (string-to-number 64bit-word-str 16))
           (64bit-signed (hexl-inspect--twos-complement 64bit-unsigned 64))
           (32bit-word-str (hexl-inspect--word-str 64bit-word-str 8 hexl-inspect--big-endian-p))
           (32bit-unsigned (string-to-number 32bit-word-str 16))
           (32bit-signed (hexl-inspect--twos-complement 32bit-unsigned 64))
           (32bit-bin-str (hexl-inspect--hex-str-to-bin 32bit-word-str))
           (32bit-char-str (hexl-inspect--decode-hex-string 32bit-word-str))
           (16bit-word-str (hexl-inspect--word-str 64bit-word-str 4 hexl-inspect--big-endian-p))
           (16bit-unsigned (string-to-number 16bit-word-str 16))
           (16bit-signed (hexl-inspect--twos-complement 16bit-unsigned 16))
           (16bit-bin-str (hexl-inspect--hex-str-to-bin 16bit-word-str))
           (16bit-char-str (hexl-inspect--decode-hex-string 16bit-word-str))
           (byte-str (hexl-inspect--word-str 64bit-word-str 2 hexl-inspect--big-endian-p))
           (byte-unsigned (string-to-number byte-str 16))
           (byte-signed (hexl-inspect--twos-complement byte-unsigned 8))
           (byte-bin-str (hexl-inspect--hex-str-to-bin byte-str))
           (byte-char-str (hexl-inspect--decode-hex-string byte-str))
           (local-endian-str hexl-inspect--endian-str))
      (with-current-buffer hexl-inspect--data-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "-------------------- Data Inspection -------------------\n")
          (insert (format "-------------------- %s -------------------\n" local-endian-str))
          (insert (format "Byte (Hex):           0x%s\n" byte-str))
          (insert (format "Byte (Binary):        0b%s\n" byte-bin-str))
          (insert (format "uint8:                %d\n" byte-unsigned))
          (insert (format "int8:                 %d\n" byte-signed))
          (insert (format "char:                 %s\n\n" byte-char-str))
          (insert (format "16-bit word (Hex):    0x%s\n" 16bit-word-str))
          (insert (format "16-bit word (Binary): 0b%s\n" 16bit-bin-str))
          (insert (format "uint16:               %d\n" 16bit-unsigned))
          (insert (format "int16:                %d\n" 16bit-signed))
          (insert (format "chars:                %s\n\n" 16bit-char-str))
          (insert (format "32-bit word (Hex):    0x%s\n" 32bit-word-str))
          (insert (format "32-bit word (Binary): 0b%s\n" 32bit-bin-str))
          (insert (format "uint32:               %d\n" 32bit-unsigned))
          (insert (format "int32:                %d\n" 32bit-signed))
          (insert (format "chars:                %s\n\n" 32bit-char-str))
          (insert (format "64-bit word (Hex):    0x%s\n" 64bit-word-str))
          (insert (format "uint64:               %d\n" 64bit-unsigned))
          (insert (format "int64:                %d\n" 64bit-signed))
          (insert "--------------------------------------------------------\n"))))))

;; Pretty much what it says on the tin, kills the data buffer for inspection.
(defun hexl-inspect--kill-inspection-buffer ()
  "Kill the inspection buffer of this hexl buffer."
  (when (buffer-live-p hexl-inspect--data-buf)
    (kill-buffer hexl-inspect--data-buf)))

;; This hook starts up the refresh timer.  If there's one that already exists,
;; kill it then start a new one.
(defun hexl-inspect--inspecting-post-command (&rest _)
  "Post-command function that runs in the source buffer."
  (when hexl-inspect-mode
    (when hexl-inspect--inspection-refresh-timer
      (cancel-timer hexl-inspect--inspection-refresh-timer))
    (setq-local hexl-inspect--inspection-refresh-timer
                (run-with-timer 0.1 nil #'hexl-inspect--inspection-refresh))))

;; A derived internal mode that defines behavior for tracking the
;; data inspection behavior.
(define-derived-mode hexl-inspect--inspecting-mode special-mode
  "Inspection"
  "Major mode for displaying hexl data inspection results."
  nil)

;; Defining a minor mode that may be able to define when certain automatic
;; behaviors should occur (like updating the inspection panel.)
;;;###autoload
(define-minor-mode hexl-inspect-mode
  "Toggle Hexl Data Inspection Mode
Interatively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode.  `toggle' toggles the state.

When Hexl Data Inspection Mode is enabled, for buffers where
hexl-mode is set, this mode will create a data inspection panel
detailing many parameters about the values at the point in the
hexl-mode buffer and will update as the point is moved.

Structure heavily borrowed from `treesit-explore-mode' in
`treesit.el'"
  :init-value nil
  :lighter " Inspection"
  :keymap
  (list (cons (kbd "C-c h") 'hexl-inspect-toggle-endian))
  ;; Body
  (if hexl-inspect-mode
      ;; Actions when asserting hexl-inspect-mode including setting up a buffer
      ;; if it doesn't exist, and setting some initial defaults.
      (progn
        (unless (buffer-live-p hexl-inspect--data-buf)
          (setq-local hexl-inspect--data-buf (get-buffer-create (format "*hexl data inspection for %s*" (buffer-name))))
          (with-current-buffer hexl-inspect--data-buf
            (hexl-inspect--inspecting-mode)))
        (display-buffer hexl-inspect--data-buf
                        (cons nil '((inhibit-same-window . t))))
        (hexl-inspect--inspection-refresh)
        ;; Setting up variables and hooks
        (setq-local hexl-inspect--big-endian-p nil)
        (setq-local hexl-inspect--endian-str " Little endian ")
        (add-hook 'post-command-hook
                  #'hexl-inspect--inspecting-post-command 0 t)
        (add-hook 'kill-buffer-hook
                  #'hexl-inspect--kill-inspection-buffer 0 t)
        ;; Telling `desktop-save' to not save explorer buffers.
        ;; (From `treesit.el' in `treesit-explore-mode')
        (when (boundp 'desktop-modes-not-to-save)
          (unless (memq 'hexl-inspect--inspecting-mode
                        desktop-modes-not-to-save)
            (push 'hexl-inspect--inspecting-mode
                  desktop-modes-not-to-save))))
    ;; Actions when deasserting hexl-inspect-mode
    (remove-hook 'post-command-hook
                 #'hexl-inspect--inspecting-post-command t)
    (remove-hook 'post-command-hook
                 #'hexl-inspect--kill-inspection-buffer t)
    ;; Destroy buffer
    (hexl-inspect--kill-inspection-buffer)))

(provide 'hexl-inspect)
