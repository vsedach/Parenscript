(in-package "COMMON-LISP")

;; These are convenience packages that export JS and browser DOM
;; symbols. If you :use the packages in a package FOO and then
;; obfuscate FOO, it will prevent the JS symbols from getting
;; mangled.

;; For most web development tasks, you want to import PS-JS-SYMBOLS,
;; PS-WINDOW-WD-SYMBOLS (which includes DOM level 2 and the w3c Window
;; working draft), and possibly the PS-DOM-NONSTANDARD-SYMBOLS.

(defpackage "PS-JS-SYMBOLS"
  (:export
   #:to-fixed
   #:encode-u-r-i-component
   #:size
   #:*array
   #:*date
   #:get-time
   #:arguments
   #:join
   #:prototype
   #:slice
   #:call
   ))

(defpackage "PS-DOM1-SYMBOLS"
  (:use "COMMON-LISP") ;; ensure we don't have naming collisions w/symbols defined in CL
  (:export
   ;;; Core
   ;; DOMImplementation
   ; methods
   #:has-feature
   
   ;; document interface
   ; attributes
   #:doctype
   #:implementation
   #:document-element
   ; methods
   #:create-element
   #:create-document-fragment
   #:create-text-node
   #:create-comment
   #:create-C-D-A-T-A-Section
   #:create-processing-instruction
   #:create-attribute
   #:create-entity-reference
   #:get-elements-by-tag-name

   ;; node interface
   ; attributes
   #:node-name
   #:node-value
   #:node-type
   #:parent-node
   #:child-nodes
   #:first-child
   #:last-child
   #:previous-sibling
   #:next-sibling
   #:attributes
   #:owner-document
   ; methods
   #:insert-before
   #:replace-child
   #:remove-child
   #:append-child
   #:has-child-nodes
   #:clone-node

   ;; nodelist interface
   ; methods
   #:item
   ; attributes
   #:length

   ;; namednodemap
   ; methods
   #:get-named-item
   #:set-named-item
   #:remove-named-item
   #:item
   ; attributes
   #:length

   ;; characterdata
   ; attributes
   #:data
   #:length
   ; methods
   #:substring-data
   #:append-data
   #:insert-data
   #:delete-data
   #:replace-data

   ;; attr
   ; attributes
   #:name
   #:specified
   #:value

   ;; element
   ; attributes
   #:tag-name
   ; methods
   #:get-attribute
   #:set-attribute
   #:remove-attribute
   #:get-attribute-node
   #:set-attribute-node
   #:remove-attribute-node
   #:get-elements-by-tag-name
   #:normalize

   ;; text
   ; methods
   #:split-text

   ;;; Level 1 extended interfaces (XML) 
   ;; DocumentType
   ; attributes
   #:name
   #:entities
   #:notations

   ;; notation
   ; attributes
   #:public-id
   #:system-id

   ;; entity
   ; attrs
   #:public-id
   #:system-id
   #:notation-name

   ;; processing instruction
   ; attrs
   #:target
   #:data

   ;;; HTML
   ;; HTMLcollection/live NodeList
   ; attributes
   #:length
   ; methods
   #:item
   #:named-item

   ;; document
   #:document
   ; attributes
   #:title
   #:referrer
   #:domain
   #:*URL*
   #:body
   #:images
   #:applets
   #:links
   #:forms
   #:anchors
   #:cookie
   ; methods
   #:open
   #:close
   #:write
   #:writeln
   #:get-element-by-id
   #:get-elements-by-name

   ;; generic HTML element
   ; attributes
   #:id
   #:title
   #:lang
   #:dir
   #:class-name

   ;; HTML document root
   ; attributes
   #:version

   ;; head
   ; attributes
   #:profile

   ;; link
   ; attributes
   #:disabled
   #:charset
   #:href
   #:hreflang
   #:media
   #:rel
   #:rev
   #:target
   #:type

   ;; title
   ; attrs
   #:text

   ;; meta
   ; attrs
   #:content
   #:http-equiv
   #:name
   #:scheme

   ;; base
   ; attrs
   #:href
   #:target

   ;; isindex
   ; attrs
   #:form
   #:prompt

   ;; style
   ; attrs
   #:disabled
   #:media
   #:type

   ;; body
   ; attrs
   #:a-link
   #:background
   #:bg-color
   #:link
   #:text
   #:v-link

   ;; form
   ; attrs
   #:elements
   #:length
   #:name
   #:accept-charset
   #:action
   #:enctype
   #:method
   #:target
   ; methods
   #:submit
   #:reset

   ;; select
   ; attrs
   #:type
   #:selected-index
   #:value
   #:length
   #:form
   #:options
   #:disabled
   #:multiple
   #:name
   #:size
   #:tab-index
   ; methods
   #:add
   #:remove
   #:blur
   #:focus

   ;; optgroup
   ; attrs
   #:disabled
   #:label

   ;; option
   ; attrs
   #:form
   #:default-selected
   #:text
   #:index
   #:disabled
   #:label
   #:selected
   #:value

   ;; input
   ; attrs
   #:default-value
   #:default-checked
   #:form
   #:accept
   #:access-key
   #:align
   #:alt
   #:checked
   #:disabled
   #:max-length
   #:name
   #:read-only
   #:size
   #:src
   #:tab-index
   #:type
   #:use-map
   #:value
   ; methods
   #:blur
   #:focus
   #:select
   #:click

   ;; textarea
   ; attrs
   #:default-value
   #:form
   #:access-key
   #:cols
   #:disabled
   #:name
   #:read-only
   #:rows
   #:tab-index
   #:type
   #:value
   ; methods
   #:blur
   #:focus
   #:select

   ;; button
   ; attrs
   #:form
   #:access-key
   #:disabled
   #:name
   #:tab-index
   #:type
   #:value

   ;; label
   ; attrs
   #:form
   #:access-key
   #:html-for

   ;; fieldset
   ; attrs
   #:form

   ;; legend
   ; attrs
   #:form
   #:access-key
   #:align

   ;; ul
   ; attrs
   #:compact
   #:type

   ;; ol
   ; attrs
   #:compact
   #:start
   #:type

   ;; dl, dir and menu
   ; attrs
   #:compact

   ;; li
   ; attrs
   #:type
   #:value

   ;; blockquote and q
   ; attrs
   #:cite

   ;; div, p, and h1/h2/hn
   ; attrs
   #:align

   ;; pre
   ; attrs
   #:width

   ;; br
   ; attrs
   #:clear

   ;; basefont, font
   ; attrs
   #:color
   #:face
   #:size

   ;; hr
   ; attrs
   #:align
   #:no-shade
   #:size
   #:width

   ;; ins and del
   ; attrs
   #:cite
   #:date-time

   ;; a
   ; attrs
   #:access-key
   #:charset
   #:coords
   #:href
   #:hreflang
   #:name
   #:rel
   #:rev
   #:shape
   #:tab-index
   #:target
   #:type
   ; methods
   #:blur
   #:focus

   ;; img
   ; attrs
   #:low-src
   #:name
   #:align
   #:alt
   #:border
   #:height
   #:hspace
   #:is-map
   #:long-desc
   #:src
   #:use-map
   #:vspace
   #:width

   ;; object
   ; attrs
   #:form
   #:code
   #:align
   #:archive
   #:border
   #:code-base
   #:code-type
   #:data
   #:declare
   #:height
   #:hspace
   #:name
   #:standby
   #:tab-index
   #:type
   #:use-map
   #:vspace
   #:width

   ;; param
   ; attrs
   #:name
   #:type
   #:value
   #:value-type

   ;; applet
   ; attrs
   #:align
   #:alt
   #:archive
   #:code
   #:code-base
   #:height
   #:hspace
   #:name
   #:object
   #:vspace
   #:width

   ;; map
   ; attrs
   #:areas
   #:name

   ;; area
   ; attrs
   #:access-key
   #:alt
   #:coords
   #:href
   #:no-href
   #:shape
   #:tab-index
   #:target

   ;; script
   ; attrs
   #:text
   #:html-for
   #:event
   #:charset
   #:defer
   #:src
   #:type

   ;; table
   ; attrs
   #:caption
   #:t-head
   #:t-foot
   #:rows
   #:t-bodies
   #:align
   #:bg-color
   #:border
   #:cell-padding
   #:cell-spacing
   #:frame
   #:rules
   #:summary
   #:width
   ; methods
   #:create-t-head
   #:delete-t-head
   #:create-t-foot
   #:delete-t-foot
   #:create-caption
   #:delete-caption
   #:insert-row
   #:delete-row

   ;; caption
   ; attrs
   #:align

   ;; col
   ; attrs
   #:align
   #:ch
   #:ch-off
   #:span
   #:v-align
   #:width

   ;; thead, tfoot, tbody
   ; attrs
   #:align
   #:ch
   #:ch-off
   #:v-align
   #:rows
   ; methods
   #:insert-row
   #:delete-row

   ;; tr
   ; attrs
   #:row-index
   #:section-row-index
   #:cells
   #:align
   #:bg-color
   #:ch
   #:ch-off
   #:v-align
   ; methods
   #:insert-cell
   #:delete-cell

   ;; th and td
   ; attrs
   #:cell-index
   #:abbr
   #:align
   #:axis
   #:bg-color
   #:ch
   #:ch-off
   #:col-span
   #:headers
   #:height
   #:no-wrap
   #:row-span
   #:scope
   #:v-align
   #:width

   ;; frameset
   ; attrs
   #:cols
   #:rows

   ;; frame
   ; attrs
   #:frame-border
   #:long-desc
   #:margin-height
   #:margin-width
   #:name
   #:no-resize
   #:scrolling
   #:src

   ;; iframe
   ; attrs
   #:align
   #:frame-border
   #:height
   #:long-desc
   #:margin-height
   #:margin-width
   #:name
   #:scrolling
   #:src
   #:width))

(defpackage "PS-DOM2-SYMBOLS"
  (:use "PS-DOM1-SYMBOLS" "COMMON-LISP")
  (:export
   ;;; Core
   ;; DOMImplementation
   ; methods
   #:create-document
   #:create-document-type

   ;; document interface
   ; methods
   #:create-attribute-n-s
   #:create-element-n-s
   #:get-element-by-id
   #:get-elements-by-tag-name-n-s
   #:import-node

   ;; node interface
   ; attributes
   #:local-name
   #:namespace-u-r-i
   #:prefix
   ; methods
   #:is-supported

   ;; named node map
   ; methods
   #:get-named-item-n-s
   #:remove-named-item-n-s
   #:set-named-item-n-s

   ;; element interface
   ; methods
   #:get-attribute-n-s
   #:get-attribute-node-n-s
   #:get-elements-by-tag-name-n-s
   #:has-attribute-n-s
   #:remove-attribute-n-s
   #:set-attribute-n-s
   #:set-attribute-node-n-s

   ;;; Level 2 extended interfaces
   ;; document type
   ; attrs
   #:internal-subset
   #:public-id
   #:system-id

   ;;; Level 2 HTML
   ;; object, frame, iframe
   ; attr
   #:content-document

   ;;; Stylesheets
   ;; stylesheet
   ; attrs
   #:disabled
   #:href
   #:media
   #:owner-node
   #:parent-style-sheet
   #:title
   #:type

   ;; media list
   ; attrs
   #:length
   #:media-text
   ; methods
   #:append-medium
   #:delete-medium
   #:item

   ;; linkstyle
   ; attrs
   #:sheet

   ;; documentstyle
   ; attrs
   #:style-sheets

   ;;; CSS
   ;; css style sheet
   ; attrs
   #:css-rules
   #:owner-rule
   ; methods
   #:delete-rule
   #:insert-rule

   ;; css rule
   ; attrs
   #:css-text
   #:parent-rule
   #:parent-style-sheet
   #:type

   ;; css style rule
   ; attrs
   #:selector-text
   #:style

   ;; css media rule
   ; attrs
   #:css-rules
   #:media
   ; methods
   #:delete-rule
   #:insert-rule

   ;; css import rule
   ; attrs
   #:href
   #:media
   #:style-sheet

   ;; css charset rule
   ; attrs
   #:encoding

   ;; css style declaration
   ; attrs
   #:css-text
   #:length
   #:parent-rule
   ; methods
   #:get-property-c-s-s-value
   #:get-property-priority
   #:get-property-value
   #:item
   #:remove-property
   #:set-property

   ;; css value
   ; attrs
   #:css-text
   #:css-value-type

   ;; css primitive value
   ; attrs
   #:primitive-type
   ; methods
   #:get-counter-value
   #:get-float-value
   #:get-r-g-b-color-value
   #:get-rect-value
   #:get-string-value
   #:set-float-value
   #:set-string-value

   ;; rgb color
   ; attrs
   #:blue
   #:green
   #:red

   ;; rectangle
   ; attrs
   #:bottom
   #:left
   #:right
   #:top

   ;; counter
   ; attrs
   #:identifier
   #:list-style
   #:separator

   ;; css views
   ; methods
   #:get-computed-style

   ;; document css
   ; methods
   #:get-override-style

   ;; css stylesheets
   ; methods
   #:create-c-s-s-style-sheet

   ;;; CSS 2.0 extended interface
   ; attributes
   #:azimuth
   #:background
   #:background-attachment
   #:background-color
   #:background-image
   #:background-position
   #:background-repeat
   #:border
   #:border-bottom
   #:border-bottom-color
   #:border-bottom-style
   #:border-bottom-width
   #:border-collapse
   #:border-color
   #:border-left
   #:border-left-color
   #:border-left-style
   #:border-left-width
   #:border-right
   #:border-right-color
   #:border-right-style
   #:border-right-width
   #:border-spacing
   #:border-style
   #:border-top
   #:border-top-color
   #:border-top-style
   #:border-top-width
   #:border-width
   #:bottom
   #:caption-side
   #:clear
   #:clip
   #:color
   #:content
   #:counter-increment
   #:counter-reset
   #:css-float
   #:cue
   #:cue-after
   #:cue-before
   #:cursor
   #:direction
   #:display
   #:elevation
   #:empty-cells
   #:font
   #:font-family
   #:font-size
   #:font-size-adjust
   #:font-stretch
   #:font-style
   #:font-variant
   #:font-weight
   #:height
   #:left
   #:letter-spacing
   #:line-height
   #:list-style
   #:list-style-image
   #:list-style-position
   #:list-style-type
   #:margin
   #:margin-bottom
   #:margin-left
   #:margin-right
   #:margin-top
   #:marker-offset
   #:marks
   #:max-height
   #:max-width
   #:min-height
   #:min-width
   #:orphans
   #:outline
   #:outline-color
   #:outline-style
   #:outline-width
   #:overflow
   #:padding
   #:padding-bottom
   #:padding-left
   #:padding-right
   #:padding-top
   #:page
   #:page-break-after
   #:page-break-before
   #:page-break-inside
   #:pause
   #:pause-after
   #:pause-before
   #:pitch
   #:pitch-range
   #:play-during
   ;; #:position in CL
   #:quotes
   #:richness
   #:right
   #:size
   #:speak
   #:speak-header
   #:speak-numeral
   #:speak-punctuation
   #:speech-rate
   #:stress
   #:table-layout
   #:text-align
   #:text-decoration
   #:text-indent
   #:text-shadow
   #:text-transform
   #:top
   #:unicode-bidi
   #:vertical-align
   #:visibility
   #:voice-family
   #:volume
   #:white-space
   #:widows
   #:width
   #:word-spacing
   #:z-index

   ;;; Events
   ;; event target interface
   ; methods
   #:add-event-listener
   #:dispatch-event
   #:remove-event-listener

   ;; event listener interface
   ; methods
   #:handle-event

   ;; Event
   ; attributes
   #:bubbles
   #:cancelable
   #:current-target
   #:event-phase
   #:target
   #:time-stamp
   #:type
   ; methods
   #:init-event
   #:prevent-default
   #:stop-propagation

   ;; document event interface
   ; methods
   #:create-event

   ;; UIEvent
   ; attributes
   #:detail
   #:view
   ; methods
   #:init-u-i-event

   ;; MouseEvent
   ; attributes
   #:alt-key
   #:button
   #:client-x
   #:client-y
   #:ctrl-key
   #:meta-key
   #:related-target
   #:screen-x
   #:screen-y
   #:shift-key
   ; methods
   #:init-mouse-event

   ;; mutation event interface
   ; attributes
   #:attr-change
   #:attr-name
   #:new-value
   #:prev-value
   #:related-node
   ; methods
   #:init-mutation-event
   )
  )

(defpackage "PS-WINDOW-WD-SYMBOLS"
  ;;; The window object (w3c working draft)
  (:use "PS-DOM2-SYMBOLS" "COMMON-LISP")
  (:export
   ; attributes
   #:window
   #:self
   #:location

   ;; location interface
   ; attrs
   #:href
   #:hash
   #:host
   #:hostname
   ;; #:pathname in CL
   #:port
   #:protocol
   ;; #:search in CL
   ; methods
   ;; #:replace in CL
   #:reload

   ;; window interface
   ; attrs
   #:parent
   #:top
   #:name
   #:frame-element
   
   ;; timers
   ; methods
   #:set-timeout
   #:set-interval
   #:clear-timeout
   #:clear-interval
   )
  )

(defpackage "PS-DOM-NONSTANDARD-SYMBOLS"
   ;;; Non-standard (incl. DOM level 0) but useful
  (:export
   #:inner-h-t-m-l
   #:onload

   #:offset-left
   #:offset-top
   #:offset-height
   #:offset-width
   
   #:offset-parent
   
   #:scroll-left
   #:scroll-top
   #:scroll-width
   #:scroll-height

   #:page-x-offset
   #:page-y-offset

   #:client-height
   #:client-width
   ))

