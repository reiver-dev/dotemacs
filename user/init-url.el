;;; init-url.el --- URL verification -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; https://url.spec.whatwg.org/#percent-encoded-bytes


(defconst my:url-charset-c0-control
  "\u0000-\u001f"
  "C0 control percent-encode set.")


(defconst my:url-charset-c0-control-space
  (concat my:url-charset-c0-control "\u0020"))


(defconst my:url-charset-control
  (concat my:url-charset-c0-control "\u007f-\u009f"))


(defconst my:url-encode-c0
  "\u0000-\u001f\u007e-\uffff"
  "C0 control percent-encode set.")


(defconst my:url-encode-fragment
  (concat my:url-encode-c0
          "\u0020\u0022\u003c\u003e\u0060")
  "Fragment percent-encode set.")


(defconst my:url-encode-path
  (concat my:url-encode-fragment
          "\u0023\u003f\u007b\u007d")
  "Path percent-encode set.")


(defconst my:url-encode-userinfo
  (concat my:url-encode-path
          "\u002f\u003a\u003b\u003d\u0040\u005b\u005c\u005d")
  "Userinfo percent-encode set.")


;; https://url.spec.whatwg.org/#host-miscellaneous

(defconst my:url-host-forbidden
  "\u0000\u0009\u000a\u000d\u0020\u0023\u0025\u002f\u003a\u003f\u0040\u005b\u005c\u005d"
  "Forbidden host code point.")


(defconst my:url-trim-chars "!\"#$%&'()*+,-./@:;<=>[\\]^_`{|}~"
  "Chars to be removed around url.")


(defconst my:url-host-name-regex
  (concat
   ;; host name
   "\\(?:[a-zA-Z\u00a1-\uffff0-9][-_]*\\)*[a-zA-Z\u00a1-\uffff0-9]+"
   ;; domain name
   "\\(?:\\.\\(?:[a-zA-Z\u00a1-\uffff0-9][-_]*\\)*[a-zA-Z\u00a1-\uffff0-9]+\\)*"
   ;; TLD
   "\\(?:\\.[a-zA-Z\u00a1-\uffff]{2,}\\)?"
   ;; trailing dot
   "\\.?")
  "RFC1034 Domain name.")


(defconst my:url-host-name-regex-full
  (concat "\\`" my:url-host-name-regex "\\'")
  "RFC1034 Domain name. Matches entire string.")


(defconst my:url-host-port-ip6-regex
  "\\`\\[\\([0-9a-zA-Z\\.:]*\\)\\]\\(:[0-9]+\\)?\\'"
  "IP6 regex within brackets and optional port value.")


(defconst my:url-host-port-ip4-regex
  "\\`\\(\\(?:[0-9]+\\.\\)\\{3\\}[0-9]+\\)\\(:[0-9]+\\)?\\'"
  "IP4 regex with address and optional port value.")


(defconst my:url-host-port-name-regex
  (concat "\\`\\(" my:url-host-name-regex "\\)\\(:[0-9]+\\)?\\'")
  "Hostname regex with domain name and optional port value.")


(defun my:url-parse-addr-ip4 (segments)
  "Convert list of SEGMENTS to IP4 number."
  (let* ((o0 (string-to-number (pop segments)))
         (o1 (string-to-number (pop segments)))
         (o2 (string-to-number (pop segments)))
         (o3 (string-to-number (pop segments))))
    (when (and
           (not segments)
           (<= 1 o0 255)
           (<= 0 o1 255)
           (<= 0 o2 255)
           (<= 0 o3 255))
      (+ (ash o0 24)
         (ash o1 16)
         (ash o2 8)
         o3))))


(defun my:url-parse-addr-ip4-str (address)
  "Convert ADDRESS string to IP4 number."
  (my:url-parse-addr-ip4 (split-string address "\\.")))


(defun my:url-parse-addr-ip6 (segments)
  "Convert list of SEGMENTS to IP6 number."
  (let ((result (make-vector 8 0))
        ;; Number of segments delimited by colon `:'
        (len (length segments))
        ;; Can be only one empty segment in the middle
        (has-empty 2)
        (iter 0))
    ;; Minumum is 3 segments ::1
    ;; Maximum is 8 segments
    (when (<= 3 len 8)
      (let ((seg (pop segments)))

        ;; Handle first segment
        (setq len (1- len))
        (if (and seg (/= 0 (length seg)))
            ;; If not empty like in ::1
            (progn
              (aset result 0 (string-to-number seg 16))
              (setq iter 1))
          ;; Jump to rest segments
          (setq iter (- 8 len)))

        (while (and segments (/= 0 has-empty))
          (setq seg (pop segments)
                len (1- len))
          (if (and seg (/= 0 (length seg)))
              (progn
                (aset result iter (string-to-number seg 16))
                (setq iter (1+ iter)))
            (setq has-empty (1- has-empty)
                  iter (- 8 len))))))
    ;; If iterated over all 8 segments
    ;; and had no more than one empty segment
    (when (and (= 8 iter) (/= 0 has-empty))
      result)))


(defun my:url-parse-addr-ip6-str (address)
  "Convert ADDRESS string to IP6 number."
  (my:url-parse-addr-ip6 (split-string address ":")))


(defun my:url-host-split (host-port)
  "Parse HOST-PORT string."
  (let ((kind
         (cond
          ((string-match my:url-host-port-ip6-regex host-port) 'ip6)
          ((string-match my:url-host-port-ip4-regex host-port) 'ip4)
          ((string-match my:url-host-port-name-regex host-port) 'name))))
    (when kind
      (let ((ab (match-beginning 1))
            (ae (match-end 1))
            (pb (match-beginning 2))
            (pe (match-end 2)))
        (list kind
              (substring host-port ab ae)
              (when (and pb pe)
                (string-to-number (substring host-port (1+ pb) pe))))))))


(defun my:url-parse-addr (kind address)
  "Parse ADDRESS string of some KIND symbol.
Supported kinds are ip4 or ip6."
  (cond
   ((eq kind 'ip4) (my:url-parse-addr-ip4-str address))
   ((eq kind 'ip6) (my:url-parse-addr-ip6-str address))
   (t address)))


(provide 'init-url)


;;; init-url.el ends here
