(require 'org)
(require 'ox-latex)
(add-to-list 'load-path "/Users/makarovd/.emacs.d/elpa/htmlize-20130207.1202")
(load-library "htmlize")

(defun do-export ()
  "invoke org latex exporter"
  (org-publish-all))

(setq org-export-with-title nil)
(setq org-html-divs '((preamble "div" "wrapper")
                     (content "section" "")
                     (postamble "div" "postamble")))
(setq org-html-toplevel-hlevel 3)
(setq org-publish-project-alist
      '(("clop"
         :base-directory        "."
         :base-extension        "org"
         :publishing-directory  "../"
         :publishing-function   org-html-publish-to-html
         :html-head-extra       "<link rel=\"stylesheet\" href=\"stylesheets/styles.css\"/>\n<link rel=\"stylesheet\" href=\"stylesheets/pygment_trac.css\"/>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, user-scalable=no\"/>"
         :html-head-include-scripts nil
         :html-preamble         "      <div>
      <!-- ============== HEADER ============== -->
      <header>
        <h1>CLOP</h1>
        <p>a language for OpenCL OPtimizations</p>
        <ul>
          <li><a href=\"https://github.com/dmakarov/clop/zipball/master\">Download <strong>ZIP File</strong></a></li>
          <li><a href=\"https://github.com/dmakarov/clop/tarball/master\">Download <strong>TAR Ball</strong></a></li>
          <li><a href=\"https://github.com/dmakarov/clop\">View On <strong>GitHub</strong></a></li>
        </ul>
      </header>
      <!-- ============== SECTION ============== -->"
         :html-postamble        "      <!-- ============== FOOTER ============== -->
      <footer>
        <p>This project is maintained by <a href=\"https://github.com/dmakarov\">dmakarov</a></p>
        <p><small>Hosted on GitHub Pages &mdash; Theme by <a href=\"https://github.com/orderedlist\">orderedlist</a></small></p>
      </footer>
    </div>
    <!-- ============== SCRIPTS ============== -->
    <script src=\"javascripts/scale.fix.js\"></script>
    <script type=\"text/javascript\">
      var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
      document.write(unescape(\"%%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%%3E%%3C/script%%3E\"));
    </script>
    <script type=\"text/javascript\">
      try {
        var pageTracker = _gat._getTracker(\"UA-33317519-1\");
        pageTracker._trackPageview();
      } catch(err) {}
    </script>"
         :section-numbers       nil
         :with-author           nil
         :with-title            nil
         :with-toc              nil)))
(setq org-publish-timestamp-directory ".org-timestamps/")

(defun org-font-lock-ensure ()
  (font-lock-fontify-buffer))

(defun common-hook-actions ()
  "Peform actions common for all programming language modes"
  (font-lock-mode 1)
  ;; Settings for face attributes:
  ;; - possible values for 'slant' attribute
  ;;   italic, oblique, normal, reverse-italic, or reverse-oblique
  ;; - possible values for 'weight' attribute
  ;;   ultra-bold, extra-bold, bold, semi-bold, normal, semi-light,
  ;;   light, extra-light, or ultra-light
  ;; - 'underline' attribute can be either nil or t
  ;; Other available attributes:
  ;; - :family, :foundry, :width, :height -- string and numeric values
  ;; - :background -- color value string as for :foreground
  ;; - :overline, :strike-through, :box, :inverse-video -- nil or t
  ;; The following faces are all available, there no other ones.
  (set-face-attribute 'font-lock-builtin-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#FFFF99")
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic
                      :weight 'normal
                      :underline nil
                      :foreground "#999999")
  (set-face-attribute 'font-lock-comment-delimiter-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#999999")
  (set-face-attribute 'font-lock-constant-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#66FFFF")
  (set-face-attribute 'font-lock-doc-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#CCCCCC")
  (set-face-attribute 'font-lock-function-name-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#26474B")
  (set-face-attribute 'font-lock-keyword-face nil
                      :slant 'normal
                      :weight 'bold
                      :underline nil
                      :foreground "#AA0D91")
  (set-face-attribute 'font-lock-negation-char-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#FF3300")
  (set-face-attribute 'font-lock-preprocessor-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#FF9999")
  (set-face-attribute 'font-lock-string-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#FFCC99")
  (set-face-attribute 'font-lock-type-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#3F6E74")
  (set-face-attribute 'font-lock-variable-name-face nil
                      :slant 'normal
                      :weight 'normal
                      :underline nil
                      :foreground "#3F6E74")
)

(add-hook 'asm-mode-hook
          (lambda () (common-hook-actions)))

(add-hook 'c-mode-hook
          (lambda () (common-hook-actions)))

(add-hook 'c++-mode-hook
          (lambda () (common-hook-actions)))

(add-hook 'd-mode-hook
          (lambda () (common-hook-actions)))
