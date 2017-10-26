(eval-when-compile (require 'cl))
(require 'ox)
(require 'ox-publish)
(require 'ox-html)

;; Define Backend  
(org-export-define-derived-backend 'docutils 'html
  :filters-alist
  '(;;(:filter-final-output . ox-docutils-filter-final-output)
    (:filter-section . ox-docutils-filter-section))
  :translate-alist
  '(
    (example-block . org-docutils-example-block)
    (special-block . org-docutils-special-block)
    (headline . org-docutils-headline)
    (inner-template . org-docutils-inner-template)
    (item . org-docutils-item)    
    (latex-environment . org-docutils-latex-environment)
    (latex-fragment . org-docutils-latex-fragment)
    (link . org-docutils-link)
    (paragraph . org-docutils-paragraph)
    (plain-list . org-docutils-plain-list)
    (quote-block . org-docutils-quote-block)
    (src-block . org-docutils-src-block)
    (subscript . org-docutils-subscript)
    (superscript . org-docutils-superscript)
    (template . org-docutils-template)
    )
  :menu-entry
  '(?d "Export to Docutils"
       ((?D "As Docutils buffer" org-docutils-export-as-docutils)
        (?d "As Docutils file" org-docutils-export-to-docutils)
        ))
  )

;; Utility functions
(defun replace-amp (str)
  "Replace & with &amp; occurring in str."
  (replace-regexp-in-string "&" "&amp;" str))
;;  (if (string-match "&" str)
;;      (replace-match "&amp;" t t str) str))

(defun replace-tag-left (newtag str)
  "Replace leading <tag> with <newtag> occurring in str."
  (if (string-match "\\`<[^>]*>" str)
      (replace-match (format "<%s>" newtag) t t str) str))

(defun replace-tag-right (newtag str)
  "Replace trailing </tag> with </newtag> occurring in str."
  (if (string-match "</.+>[^>]*\\'" str)
      (replace-match (format "</%s> " newtag) t t str) str))

(defun replace-tag (newtag str)
  "Replace tag with newtag occurring in str."
  (replace-tag-left newtag (replace-tag-right newtag str)))

(defun remove-tag-left (str)
  "Remove leading <tag> occurring in str."
  (if (string-match "\\`<[^>]*>" str)
      (replace-match "" t t str) str))

(defun remove-tag-right (str)
  "Remove trailing </tag> occurring in str."
  (if (string-match "</.+>[^>]*\\'" str)
      (replace-match "" t t str) str))

(defun remove-tag (str)
  "Remove tag occurring in str."
  (remove-tag-left (remove-tag-right str)))

(defun remove-inline-tex-left (str)
  "Remove leading tex occurring in str."
  (if (or(string-match "\\`\\$" str)
         (string-match "\\`\\\\(" str))
      (replace-match "" t t str) str))

(defun remove-tex-left (str)
  "Remove leading tex occurring in str."
  (if (or(string-match "\\`\\$\\$?" str)
         (string-match "\\`\\\\[[(]" str))
      (replace-match "" t t str) str))

(defun remove-tex-right (str)
  "Remove trailing tex occurring in str."
  (if (or(string-match "\\$\\$? *\\'" str)
         (string-match "\\\\[])] *\\'" str))
      (replace-match "" t t str) str))

(defun remove-tex (str)
  "Remove tex occurring in str."
  (remove-tex-left (remove-tex-right str)))

;; Bold, etc.
(customize-set-variable 'org-html-text-markup-alist
  '((bold . "<strong>%s</strong>")
    (code . "<literal>%s</literal>")
    (italic . "<emphasis>%s</emphasis>")
    (strike-through . "%s")
    (underline . "%s")
    (verbatim . "<literal>%s</literal>")))

(customize-set-variable 'org-html-container-element "section")

(customize-set-variable 'org-html-format-headline-function
                        'org-docutils-format-headline-function)

(customize-set-variable 'org-html-htmlize-output-type nil)

;; Internal functions
(defun org-docutils--format-image (source attributes info)
  "\"img\" tags in html are \"image\" tags in docutils"
  (replace-regexp-in-string "<img src=" "<image uri="
    (org-html--format-image (source attributes info))))

(defun org-docutils--wrap-image (contents info &optional caption label)
  "\"img\" tags in html are \"image\" tags in docutils"
  (format "<figure alt=\"%s\" target=\"%s\">%s</figure>" caption label contents))

(defun org-docutils-format-code (element info)
  "Format contents of ELEMENT as source code. Not sure about so-colled \"noweb\" things."
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element)))
	 ;; Does it have line numbers?
         ;;(num-start (org-export-get-loc element info)))
    (format "<literal_block classes=\"code %s\">%s</literal_block>" lang code)))

(defun org-docutils-format-headline-function
    (todo todo-type priority text tags)
  "Bug: It should take 6 args, but complains the 6th."
  text)

;; Transcode Functions

;;;; Template
(defun org-docutils-inner-template (contents info)
  "Ignore info, just contents"
  contents)

(defun org-docutils-template (contents info)
  "Ignore info, just contents"
  (concat
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE document PUBLIC \"+//IDN docutils.sourceforge.net//DTD Docutils Generic//EN//XML\" \"http://docutils.sourceforge.net/docs/ref/docutils.dtd\">
<!-- Generated by ox-docutils 0.0.1 -->
<document>\n"
   ;(when (plist-get info :with-title)
     (let ((title (plist-get info :title))
	   (subtitle (plist-get info :subtitle)))
       (when title
         (format "<title>%s</title>%s"
                 (org-export-data title info)
                 (if subtitle
                     (format "<subtitle>%s</subtitle>"
                             (org-export-data subtitle info)) ""))));)
   contents
   "</document>"))

;;;; Example Block
(defun org-docutils-example-block (example-block _contents info)
  "Transcode a EXAMPLEBLOCK object from Org to docutils."
  (format "<literal_block>%s</literal_block>" (org-html-format-code example-block info)))

;;;; Special Block
(defun org-docutils-special-block (special-block contents info)
    "Transcode a SPECIAL-BLOCK element from Org to docutils.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
    (let* ((block-type (org-element-property :type special-block))
           (attributes (org-export-read-attribute :attr_html special-block)))
      (let ((class (plist-get attributes :class)))
        (setq attributes (plist-put attributes :class
                                    (if class (concat class " " block-type)
                                      block-type))))
      (let* ((contents (or contents ""))
             (name (org-element-property :name special-block))
             (a (org-html--make-attribute-string
                 (if (or (not name) (plist-member attributes :id))
                     attributes
                   (plist-put attributes :id name))))
             (str (if (org-string-nw-p a) (concat " " a) "")))
        (format "<admonition%s>\n%s\n</admonition>" str contents))))

;;;; Headline
(defun org-docutils-headline (headline contents info)
  "Set ids attr."
  (let ((ids (org-export-data (org-element-property :title headline) info)))
    (format "<section ids=\"%s\"><title>%s</title>%s</section>" ids ids contents)))

;;;; Item
(defun org-docutils-item (item contents info)
  "Transcode an ITEM element from Org to docutils."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list)))
    (case type
      (ordered (format "<list_item>%s</list_item>" contents))
      (unordered (format "<list_item>%s</list_item>" contents))
      (descriptive
       (format
        "<definition_list_item><term>%s</term><definition>%s</definition></definition_list_item>"
        (org-export-data (org-element-property :tag item) info) contents)))))

;;;; Latex Environment
(defun org-docutils-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to docutils."
  (let ((latex-frag (replace-amp
                     (org-remove-indentation
                      (org-element-property :value latex-environment)))))
    (format "<math_block>%s</math_block>" latex-frag)))

;;;; Latex Fragment
(defun org-docutils-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT element from Org to docutils."
  (let ((latex-frag (replace-amp
                     (org-remove-indentation
                      (org-element-property :value latex-fragment)))))
    (format "<math>%s</math>" (remove-tex latex-frag))))

;;;; Link
(defun org-docutils-link (link desc info)
  (let ((html-out (org-html-link link desc info))
        (type (org-element-property :type link)))
    (cond ((org-html-inline-image-p link info)
           (replace-regexp-in-string "<img src=" "<image uri=" html-out))
          (t (replace-regexp-in-string "<a href=" "<reference refuri="
                                       (replace-tag-right "reference" html-out))))))
;;;; Paragraph
(defun org-docutils-paragraph (paragraph contents info)
  "standalone image"
  (if (org-html-standalone-image-p paragraph info)
      (let ((caption (org-export-data
			 (org-export-get-caption paragraph) info))
	    (label (org-element-property :name paragraph)))
			;(org-export-get-reference paragraph info)))
        (org-docutils--wrap-image contents info caption label))
  "otherwise"
    (format "<paragraph>%s</paragraph>" contents)))

;;;; Plain List
(defun org-docutils-type-plain-list (type)
  "Insert the end of the docutils list depending on TYPE."
  (case type
    (ordered "enumerated_list")
    (unordered "bullet_list")
    (descriptive "definition_list")))

(defun org-docutils-plain-list (plain-list contents info)
  "plain list is one of 'ol', 'ul' or 'dl'"
  (let* (arg1 ;; (assoc :counter (org-element-map plain-list 'item
	 (type (org-element-property :type plain-list)))
    (format "<%s>%s</%s>"
            (org-docutils-type-plain-list type)
            contents
            (org-docutils-type-plain-list type))))

;;;; Quote Block
(defun org-docutils-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to docutils."
  (format "<block_quote>\n%s</block_quote>" contents))

;;;; Src Block
(defun org-docutils-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to docutils."
  (let* ((lang (org-element-property :language src-block))
         (code (org-docutils-format-code src-block info))
         (label (let ((lbl (and (org-element-property :name src-block)
                                (org-export-get-reference src-block info))))
                  (if lbl lbl ""))))
    code))

;;;; Subscript
(defun org-docutils-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to docutils."
  (format "<subscript>%s</subscript>" contents))

;;;; Superscript
(defun org-docutils-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to docutils."
  (format "<superscript>%s</superscript>" contents))

;; Filter Functions

;;;; Section
(defun ox-docutils-filter-section (text back-end info)
  "<div class=\"outline-text-%d\" id=\"text-%s\">\nSECTION</div>"
  (remove-tag text))

;;;; Final Output
;;(defun ox-docutils-filter-final-output (text back-end info)
  ;;"<document>TEXT</document>"
  ;;(format "<document>%s</document>" text))
  ;;text)

;; End-user functions
(defun org-docutils-export-as-docutils
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an docutils buffer."
  (interactive)
  (org-export-to-buffer 'docutils "*Org Docutils Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

(defun org-docutils-export-to-docutils
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension ".xml")
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'docutils file
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-docutils)

;; ox-docutils.el ends here
