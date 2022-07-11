;; latex things
(use-package ivy-bibtex
  :after ivy
  :custom
  (bibtex-completion-bibliography "~/icloud/its_lit_fam/bib.bib")
  (bibtex-completion-library-path "~/icloud/its_lit_fam/bibtex_pdfs/")
  ;; (bibtex-completion-library-path '("/path1/to/pdfs" "/path2/to/pdfs"))
  ;; (bibtex-completion-notes-path "~/icloud/its_lit_fam/bib.org")
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "_")
  (bibtex-autokey-titlewords 1)
  (bibtex-autokey-titleword-stretch 1)
  (bibtex-autokey-titleword-length 6)
  (bibtex-completion-cite-prompt-for-optional-arguments nil))

(use-package org-ref
  :custom
  (org-ref-default-bibliography '("~/icloud/its_lit_fam/bib.bib"))
  (org-ref-bibliography-notes "~/icloud/its_lit_fam/bib.org")
  (org-ref-pdf-directory "~/icloud/its_lit_fam/bibtex_pdfs/")
  )
  
(cole/local-leader-keys bibtex-mode-map
  "b" '(ivy-bibtex :which-key "ivy-bibtex")
  "o" '(:ignore t "open")
  "ob" '(org-ref-open-in-browser :which-key "open in browser")
  "op" '(org-ref-open-bibtex-pdf :which-key "open associated pdf")
  "oP" '(org-ref-bibtex-pubmed :which-key "open in pubmed")
  "og" '(org-ref-bibtex-google-scholar :which-key "open in google scholar")
  "A" '(org-ref-assoc-pdf-with-entry :which-key "associate pdf")
  "C" '(bibtex-clean-entry :which-key "clean entry")
  "H" '(org-ref-bibtex-hydra/body :which-key "bibtex hydra")
  "n" '(org-ref-open-bibtex-notes :which-key "open notes")
  "t" '(helm-tag-bibtex-entry :which-key "tag")
  "i" '(:ignore t "insert entry")
  "ii" '(doi-utils-add-entry-from-crossref-query :which-key "crossref")
  "ip" '(pubmed-insert-bibtex-from-pmid :which-key "PMID")
  "id" '(doi-utils-add-bibtex-entry-from-doi :which-key "DOI")
  )

(provide 'colemacs-latex)
