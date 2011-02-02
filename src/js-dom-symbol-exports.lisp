(in-package #:parenscript)
(in-readtable :parenscript)

;; These are convenience packages that export JS and browser DOM
;; symbols. If you :use the packages in a package FOO and then
;; obfuscate FOO, it will prevent the JS symbols from getting
;; mangled.

;; For most web development tasks, you want to import PS-JS-SYMBOLS,
;; PS-WINDOW-WD-SYMBOLS (which includes DOM level 2 and the w3c Window
;; working draft), and possibly the PS-DOM-NONSTANDARD-SYMBOLS.

(defun re-export-symbols (from-package to-package)
  (do-external-symbols (symbol from-package)
    (multiple-value-bind (cl-symbol type) (find-symbol (symbol-name symbol) '#:cl)
      (when (eq type :external)
        (shadowing-import cl-symbol from-package)))
    (shadowing-import symbol to-package)
    (export (list symbol) to-package)))

(defpackage #:ps-js-symbols
  (:documentation "JavaScript standard function and property names.")
  (:export
   #:to-fixed                      #:toFixed
   #:encode-u-r-i-component        #:encodeURIComponent
   #:size
   #:*array                        #:Array
   #:*date                         #:Date
   #:get-time                      #:getTime
   #:arguments
   #:join
   #:prototype
   #:slice
   #:call
   ))

(re-export-symbols '#:ps-js-symbols '#:parenscript)

(defpackage #:ps-dom1-symbols
  (:documentation "DOM Level 1 symbols.")
  (:export
   ;;; Core
   ;; DOMImplementation
   ; methods
   #:has-feature                   #:hasFeature

   ;; document interface
   ; attributes
   #:doctype
   #:implementation
   #:document-element              #:documentElement
   ; methods
   #:create-element                #:createElement
   #:create-document-fragment      #:createDocumentElement
   #:create-text-node              #:createTextNode
   #:create-comment                #:createComment
   #:create-c-d-a-t-a-section      #:createCDATASection
   #:create-processing-instruction #:createProcessingInstruction
   #:create-attribute              #:createAttribute
   #:create-entity-reference       #:createEntityReference
   #:get-elements-by-tag-name      #:getElementsByTagName

   ;; node interface
   ; attributes
   #:node-name                     #:nodeName
   #:node-value                    #:nodeValue
   #:node-type                     #:nodeType
   #:parent-node                   #:parentNode
   #:child-nodes                   #:childNodes
   #:first-child                   #:firstChild
   #:last-child                    #:lastChild
   #:previous-sibling              #:previousSibling
   #:next-sibling                  #:nextSibling
   #:attributes
   #:owner-document                #:ownerDocument
   ; methods
   #:insert-before                 #:insertBefore
   #:replace-child                 #:replaceChild
   #:remove-child                  #:removeChild
   #:append-child                  #:appendChild
   #:has-child-nodes               #:hasChildNodes
   #:clone-node                    #:cloneNode

   ;; nodelist interface
   ; methods
   #:item
   ; attributes
   #:length

   ;; namednodemap
   ; methods
   #:get-named-item                #:getNamedItem
   #:set-named-item                #:setNamedItem
   #:remove-named-item             #:removeNamedItem
   #:item
   ; attributes
   #:length

   ;; characterdata
   ; attributes
   #:data
   #:length
   ; methods
   #:substring-data                #:substringData
   #:append-data                   #:appendData
   #:insert-data                   #:insertData
   #:delete-data                   #:deleteData
   #:replace-data                  #:replaceData

   ;; attr
   ; attributes
   #:name
   #:specified
   #:value

   ;; element
   ; attributes
   #:tag-name                      #:tagName
   ; methods
   #:get-attribute                 #:getAttribute
   #:set-attribute                 #:setAttribute
   #:remove-attribute              #:removeAttribute
   #:get-attribute-node            #:getAttributeNode
   #:set-attribute-node            #:setAttributeNode
   #:remove-attribute-node         #:removeAttributeNode
   #:get-elements-by-tag-name      #:getElementsByTagName
   #:normalize

   ;; text
   ; methods
   #:split-text                    #:splitText

   ;;; Level 1 extended interfaces (XML)
   ;; DocumentType
   ; attributes
   #:name
   #:entities
   #:notations

   ;; notation
   ; attributes
   #:public-id                     #:publicId
   #:system-id                     #:systemId

   ;; entity
   ; attrs
   #:public-id                     #:publicId
   #:system-id                     #:systemId
   #:notation-name                 #:notationName

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
   #:named-item                    #:namedItem

   ;; document
   #:document
   ; attributes
   #:title
   #:referrer
   #:domain
   #:*url*                         #:URL
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
   #:get-element-by-id             #:getElementById
   #:get-elements-by-name          #:getElementsByName

   ;; generic HTML element
   ; attributes
   #:id
   #:title
   #:lang
   #:dir
   #:class-name                    #:className

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
   #:http-equiv                    #:httpEquiv
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
   #:a-link                        #:aLink
   #:background
   #:bg-color                      #:bgColor
   #:link
   #:text
   #:v-link                        #:vLink

   ;; form
   ; attrs
   #:elements
   #:length
   #:name
   #:accept-charset                #:acceptCharset
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
   #:selected-index                #:selectedIndex
   #:value
   #:length
   #:form
   #:options
   #:disabled
   #:multiple
   #:name
   #:size
   #:tab-index                     #:tabIndex
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
   #:default-selected              #:defaultSelected
   #:text
   #:index
   #:disabled
   #:label
   #:selected
   #:value

   ;; input
   ; attrs
   #:default-value                 #:defaultValue
   #:default-checked               #:defaultChecked
   #:form
   #:accept
   #:access-key                    #:accessKey
   #:align
   #:alt
   #:checked
   #:disabled
   #:max-length                    #:maxLength
   #:name
   #:read-only                     #:readOnly
   #:size
   #:src
   #:tab-index                     #:tabIndex
   #:type
   #:use-map                       #:useMap
   #:value
   ; methods
   #:blur
   #:focus
   #:select
   #:click

   ;; textarea
   ; attrs
   #:default-value                 #:defaultValue
   #:form
   #:access-key                    #:accessKey
   #:cols
   #:disabled
   #:name
   #:read-only                     #:readOnly
   #:rows
   #:tab-index                     #:tabIndex
   #:type
   #:value
   ; methods
   #:blur
   #:focus
   #:select

   ;; button
   ; attrs
   #:form
   #:access-key                    #:accessKey
   #:disabled
   #:name
   #:tab-index                     #:tabIndex
   #:type
   #:value

   ;; label
   ; attrs
   #:form
   #:access-key                    #:accessKey
   #:html-for                      #:htmlFor

   ;; fieldset
   ; attrs
   #:form

   ;; legend
   ; attrs
   #:form
   #:access-key                    #:accessKey
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
   #:no-shade                      #:noShade
   #:size
   #:width

   ;; ins and del
   ; attrs
   #:cite
   #:date-time                     #:dateTime

   ;; a
   ; attrs
   #:access-key                    #:accessKey
   #:charset
   #:coords
   #:href
   #:hreflang
   #:name
   #:rel
   #:rev
   #:shape
   #:tab-index                     #:tabIndex
   #:target
   #:type
   ; methods
   #:blur
   #:focus

   ;; img
   ; attrs
   #:low-src                       #:lowSrc
   #:name
   #:align
   #:alt
   #:border
   #:height
   #:hspace
   #:is-map                        #:isMap
   #:long-desc                     #:longDesc
   #:src
   #:use-map                       #:useMap
   #:vspace
   #:width

   ;; object
   ; attrs
   #:form
   #:code
   #:align
   #:archive
   #:border
   #:code-base                     #:codeBase
   #:code-type                     #:codeType
   #:data
   #:declare
   #:height
   #:hspace
   #:name
   #:standby
   #:tab-index                     #:tabIndex
   #:type
   #:use-map                       #:useMap
   #:vspace
   #:width

   ;; param
   ; attrs
   #:name
   #:type
   #:value
   #:value-type                    #:valueType

   ;; applet
   ; attrs
   #:align
   #:alt
   #:archive
   #:code
   #:code-base                     #:codeBase
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
   #:access-key                    #:accessKey
   #:alt
   #:coords
   #:href
   #:no-href                       #:noHref
   #:shape
   #:tab-index                     #:tabIndex
   #:target

   ;; script
   ; attrs
   #:text
   #:html-for                      #:htmlFor
   #:event
   #:charset
   #:defer
   #:src
   #:type

   ;; table
   ; attrs
   #:caption
   #:t-head                        #:tHead
   #:t-foot                        #:tFoot
   #:rows
   #:t-bodies                      #:tBodies
   #:align
   #:bg-color                      #:bgColor
   #:border
   #:cell-padding                  #:cellPadding
   #:cell-spacing                  #:cellSpacing
   #:frame
   #:rules
   #:summary
   #:width
   ; methods
   #:create-t-head                 #:createTHead
   #:delete-t-head                 #:deleteTHead
   #:create-t-foot                 #:createTFoot
   #:delete-t-foot                 #:deleteTFoot
   #:create-caption                #:createCaption
   #:delete-caption                #:deleteCaption
   #:insert-row                    #:insertRow
   #:delete-row                    #:deleteRow

   ;; caption
   ; attrs
   #:align

   ;; col
   ; attrs
   #:align
   #:ch
   #:ch-off                        #:chOff
   #:span
   #:v-align                       #:vAlign
   #:width

   ;; thead, tfoot, tbody
   ; attrs
   #:align
   #:ch
   #:ch-off                        #:chOff
   #:v-align                       #:vAlign
   #:rows
   ; methods
   #:insert-row                    #:insertRow
   #:delete-row                    #:deleteRow

   ;; tr
   ; attrs
   #:row-index                     #:rowIndex
   #:section-row-index             #:sectionRowIndex
   #:cells
   #:align
   #:bg-color                      #:bgColor
   #:ch
   #:ch-off                        #:chOff
   #:v-align                       #:vAlign
   ; methods
   #:insert-cell                   #:insertCell
   #:delete-cell                   #:deleteCell

   ;; th and td
   ; attrs
   #:cell-index                    #:cellIndex
   #:abbr
   #:align
   #:axis
   #:bg-color                      #:bgColor
   #:ch
   #:ch-off                        #:chOff
   #:col-span                      #:colSpan
   #:headers
   #:height
   #:no-wrap                       #:noWrap
   #:row-span                      #:rowSpan
   #:scope
   #:v-align                       #:vAlign
   #:width

   ;; frameset
   ; attrs
   #:cols
   #:rows

   ;; frame
   ; attrs
   #:frame-border                  #:frameBorder
   #:long-desc                     #:longDesc
   #:margin-height                 #:marginHeight
   #:margin-width                  #:marginWidth
   #:name
   #:no-resize                     #:noResize
   #:scrolling
   #:src

   ;; iframe
   ; attrs
   #:align
   #:frame-border                  #:frameBorder
   #:height
   #:long-desc                     #:longDesc
   #:margin-height                 #:marginHeight
   #:margin-width                  #:marginWidth
   #:name
   #:scrolling
   #:src
   #:width
   ))

(defpackage #:ps-dom2-symbols
  (:documentation "DOM Level 2 symbols. Includes DOM Level 1 symbols.")
  (:export
   ;;; Core
   ;; DOMImplementation
   ; methods
   #:create-document               #:createDocument
   #:create-document-type          #:createDocumentType

   ;; document interface
   ; methods
   #:create-attribute-n-s          #:createAttributeNS
   #:create-element-n-s            #:createElementNS
   #:get-element-by-id             #:getElementById
   #:get-elements-by-tag-name-n-s  #:getElementsByTagNameNS
   #:import-node                   #:importNode

   ;; node interface
   ; attributes
   #:local-name                    #:localName
   #:namespace-u-r-i               #:namespaceURI
   #:prefix
   ; methods
   #:is-supported                  #:isSupported

   ;; named node map
   ; methods
   #:get-named-item-n-s            #:getNamedItemNS
   #:remove-named-item-n-s         #:removeNamedItermNS
   #:set-named-item-n-s            #:setNamedItemNS

   ;; element interface
   ; methods
   #:get-attribute-n-s             #:getAttributeNS
   #:get-attribute-node-n-s        #:getAttributeNodeNS
   #:get-elements-by-tag-name-n-s  #:getElementsByTagNameNS
   #:has-attribute-n-s             #:hasAttributeNS
   #:remove-attribute-n-s          #:removeAttributeNS
   #:set-attribute-n-s             #:setAttributeNS
   #:set-attribute-node-n-s        #:setAttributeNodeNS

   ;;; Level 2 extended interfaces
   ;; document type
   ; attrs
   #:internal-subset               #:internalSubset
   #:public-id                     #:publicId
   #:system-id                     #:systemId

   ;;; Level 2 HTML
   ;; object, frame, iframe
   ; attr
   #:content-document              #:contentDocument

   ;;; Stylesheets
   ;; stylesheet
   ; attrs
   #:disabled
   #:href
   #:media
   #:owner-node                    #:ownerNode
   #:parent-style-sheet            #:parenStyleSheet
   #:title
   #:type

   ;; media list
   ; attrs
   #:length
   #:media-text                    #:mediaText
   ; methods
   #:append-medium                 #:appendMedium
   #:delete-medium                 #:deleteMedium
   #:item

   ;; linkstyle
   ; attrs
   #:sheet

   ;; documentstyle
   ; attrs
   #:style-sheets                  #:styleSheets

   ;;; CSS
   ;; css style sheet
   ; attrs
   #:css-rules                     #:cssRules
   #:owner-rule                    #:ownerRule
   ; methods
   #:delete-rule                   #:deleteRule
   #:insert-rule                   #:insertRule

   ;; css rule
   ; attrs
   #:css-text                      #:cssText
   #:parent-rule                   #:parentRule
   #:parent-style-sheet            #:parentStyleSheet
   #:type

   ;; css style rule
   ; attrs
   #:selector-text                 #:selectorText
   #:style

   ;; css media rule
   ; attrs
   #:css-rules                     #:cssRules
   #:media
   ; methods
   #:delete-rule                   #:deleteRule
   #:insert-rule                   #:insertRule

   ;; css import rule
   ; attrs
   #:href
   #:media
   #:style-sheet                   #:styleSheet

   ;; css charset rule
   ; attrs
   #:encoding

   ;; css style declaration
   ; attrs
   #:css-text                      #:cssText
   #:length
   #:parent-rule                   #:parentRule
   ; methods
   #:get-property-c-s-s-value      #:getPropertyCSSValue
   #:get-property-priority         #:getPropertyPriority
   #:get-property-value            #:getPropertyValue
   #:item
   #:remove-property               #:removeProperty
   #:set-property                  #:setProperty

   ;; css value
   ; attrs
   #:css-text                      #:cssText
   #:css-value-type                #:cssValueType

   ;; css primitive value
   ; attrs
   #:primitive-type                #:primitiveType
   ; methods
   #:get-counter-value             #:getCounterValue
   #:get-float-value               #:getFloatValue
   #:get-r-g-b-color-value         #:getRGBColorValue
   #:get-rect-value                #:getRectValue
   #:get-string-value              #:getStringValue
   #:set-float-value               #:setFloatValue
   #:set-string-value              #:setStringValue

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
   #:list-style                    #:listStyle
   #:separator

   ;; css views
   ; methods
   #:get-computed-style            #:getComputedStyle

   ;; document css
   ; methods
   #:get-override-style            #:getOverrideStyle

   ;; css stylesheets
   ; methods
   #:create-c-s-s-style-sheet      #:createCSSStyleSheet

   ;;; CSS 2.0 extended interface
   ; attributes
   #:azimuth
   #:background
   #:background-attachment         #:backgroundAttachment
   #:background-color              #:backgroundColor
   #:background-image              #:backgroundImage
   #:background-position           #:backgroundPosition
   #:background-repeat             #:backgroundRepeat
   #:border
   #:border-bottom                 #:borderBottom
   #:border-bottom-color           #:borderBottomColor
   #:border-bottom-style           #:borderBottomStyle
   #:border-bottom-width           #:borderBottomWidth
   #:border-collapse               #:borderCollapse
   #:border-color                  #:borderColor
   #:border-left                   #:borderLeft
   #:border-left-color             #:borderLeftColor
   #:border-left-style             #:borderLeftStyle
   #:border-left-width             #:borderLeftWidth
   #:border-right                  #:borderRight
   #:border-right-color            #:borderRightColor
   #:border-right-style            #:borderRightStyle
   #:border-right-width            #:borderRightWidth
   #:border-spacing                #:borderSpacing
   #:border-style                  #:borderStyle
   #:border-top                    #:borderTop
   #:border-top-color              #:borderTopColor
   #:border-top-style              #:borderTopStyle
   #:border-top-width              #:borderTopWidth
   #:border-width                  #:borderWidth
   #:bottom
   #:caption-side                  #:captionSide
   #:clear
   #:clip
   #:color
   #:content
   #:counter-increment             #:counterIncrement
   #:counter-reset                 #:counterReset
   #:css-float                     #:cssFloat
   #:cue
   #:cue-after                     #:cueAfter
   #:cue-before                    #:cueBefore
   #:cursor
   #:direction
   #:display
   #:elevation
   #:empty-cells                   #:emptyCells
   #:font
   #:font-family                   #:fontFamily
   #:font-size                     #:fontSize
   #:font-size-adjust              #:fontSizeAdjust
   #:font-stretch                  #:fontStretch
   #:font-style                    #:fontStyle
   #:font-variant                  #:fontVariant
   #:font-weight                   #:fontWeight
   #:height
   #:left
   #:letter-spacing                #:letterSpacing
   #:line-height                   #:lineHeight
   #:list-style                    #:listStyle
   #:list-style-image              #:listStyleImage
   #:list-style-position           #:listStylePosition
   #:list-style-type               #:listStyleType
   #:margin
   #:margin-bottom                 #:marginBottom
   #:margin-left                   #:marginLeft
   #:margin-right                  #:marginRight
   #:margin-top                    #:marginTop
   #:marker-offset                 #:markerOffset
   #:marks
   #:max-height                    #:maxHeight
   #:max-width                     #:maxWidth
   #:min-height                    #:minHeight
   #:min-width                     #:minWidth
   #:orphans
   #:outline
   #:outline-color                 #:outlineColor
   #:outline-style                 #:outlineStyle
   #:outline-width                 #:outlineWidth
   #:overflow
   #:padding
   #:padding-bottom                #:paddingBottom
   #:padding-left                  #:paddingLeft
   #:padding-right                 #:paddingRight
   #:padding-top                   #:paddingTop
   #:page
   #:page-break-after              #:pageBreakAfter
   #:page-break-before             #:pageBreakBefore
   #:page-break-inside             #:pageBreakInside
   #:pause
   #:pause-after                   #:pauseAfter
   #:pause-before                  #:pauseBefore
   #:pitch
   #:pitch-range                   #:pitchRange
   #:play-during                   #:playDuring
   ;; #:position in CL
   #:quotes
   #:richness
   #:right
   #:size
   #:speak
   #:speak-header                  #:speakHeader
   #:speak-numeral                 #:speakNumeral
   #:speak-punctuation             #:speakPunctuation
   #:speech-rate                   #:speechRate
   #:stress
   #:table-layout                  #:tableLayout
   #:text-align                    #:textAlign
   #:text-decoration               #:textDecoration
   #:text-indent                   #:textIndent
   #:text-shadow                   #:textShadow
   #:text-transform                #:textTransform
   #:top
   #:unicode-bidi                  #:unicodeBidi
   #:vertical-align                #:verticalAlign
   #:visibility
   #:voice-family                  #:voiceFamily
   #:volume
   #:white-space                   #:whiteSpace
   #:widows
   #:width
   #:word-spacing                  #:wordSpacing
   #:z-index                       #:zIndex

   ;;; Events
   ;; event target interface
   ; methods
   #:add-event-listener            #:addEventListener
   #:dispatch-event                #:dispatchEvent
   #:remove-event-listener         #:removeEventListener

   ;; event listener interface
   ; methods
   #:handle-event                  #:handleEvent

   ;; Event
   ; attributes
   #:bubbles
   #:cancelable
   #:current-target                #:currentTarget
   #:event-phase                   #:eventPhase
   #:target
   #:time-stamp                    #:timeStamp
   #:type
   ; methods
   #:init-event                    #:initEvent
   #:prevent-default               #:preventDefault
   #:stop-propagation              #:stopPropagation

   ;; document event interface
   ; methods
   #:create-event                  #:createEvent

   ;; UIEvent
   ; attributes
   #:detail
   #:view
   ; methods
   #:init-u-i-event                #:initUIEvent

   ;; MouseEvent
   ; attributes
   #:alt-key                       #:altKey
   #:button
   #:client-x                      #:clientX
   #:client-y                      #:clientY
   #:ctrl-key                      #:ctrlKey
   #:meta-key                      #:metaKey
   #:related-target                #:relatedTarget
   #:screen-x                      #:screenX
   #:screen-y                      #:screenY
   #:shift-key                     #:shiftKey
   ; methods
   #:init-mouse-event              #:initMouseEvent

   ;; mutation event interface
   ; attributes
   #:attr-change                   #:attrChange
   #:attr-name                     #:attrName
   #:new-value                     #:newValue
   #:prev-value                    #:prevValue
   #:related-node                  #:relatedNode
   ; methods
   #:init-mutation-event           #:initMutationEvent
   ))

(re-export-symbols '#:ps-dom1-symbols '#:ps-dom2-symbols)

(defpackage #:ps-window-wd-symbols
  (:documentation "Funtions and properties of the W3C Working Draft Window object. Includes DOM level 2 symbols.")
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
   #:frame-element                 #:frameElement

   ;; timers
   ; methods
   #:set-timeout                   #:setTimeout
   #:set-interval                  #:setInterval
   #:clear-timeout                 #:clearTimeout
   #:clear-interval                #:clearInterval
   ))

(re-export-symbols '#:ps-dom2-symbols '#:ps-window-wd-symbols)

(defpackage #:ps-dom-nonstandard-symbols
  (:documentation "Non-W3C-standard (incl. DOM level 0) but widely implemented functions and properties.")
  (:export
   #:inner-h-t-m-l                 #:innerHTML
   #:onload

   #:offset-left                   #:offsetLeft
   #:offset-top                    #:offsetTop
   #:offset-height                 #:offsetHeight
   #:offset-width                  #:offsetWidth

   #:offset-parent                 #:offsetParent

   #:scroll-left                   #:scrollLeft
   #:scroll-top                    #:scrollTop
   #:scroll-width                  #:scrollWidth
   #:scroll-height                 #:scrollHeight

   #:page-x-offset                 #:pageXOffset
   #:page-y-offset                 #:pageYOffset

   #:client-height                 #:clientHeight
   #:client-width                  #:clientWidth
   ))

(defpackage #:ps-dhtml-symbols
  (:documentation "Meta-package containing function/property symbols
  of typical HTML4/DHTML browsers. Includes DOM Levels 0 to 2, Window
  working draft, and de-facto standard symbols.

This is probably the package you want to :USE if you're doing
obfuscation in Parenscript and want to target the pre-HTML5 generation
of browsers."))

(re-export-symbols '#:ps-window-wd-symbols '#:ps-dhtml-symbols)
(re-export-symbols '#:ps-dom-nonstandard-symbols '#:ps-dhtml-symbols)
