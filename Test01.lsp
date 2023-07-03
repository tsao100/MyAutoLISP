; Version 1.0

(defun c:makeLine() ; by tsao100
  (command "line" '(0 0) '(5 5) "")
)

(defun c:makeArc() ; by JackInGmail
  (command "arc" '(0 0) '(5 5) '(10 3))
  (princ)
)

(defun c:makeRect() ; by tsao100
  (command "rectang" '(0 0) '(5 5))
)

(defun c:gapes()
  (command "break" (getxy 14.85 0.3) (getxy 15.15 0.3))
)

(defun c:makePline()
  (setq polylineData
        '(
          (0 . "LWPOLYLINE")
          (100 . "AcDbEntity")
          (100 . "AcDbPolyline")
          (8 . "0")
          (90 . 4)         ; Number of vertices
          (10 0.0 20 0.0)  ; First vertex (X, Y)
          (10 20 20 0.0)  ; Second vertex (X, Y)
	  (42 . -0.5)
          (10 20 0 0.0)  ; Third vertex (X, Y)
          (10 0.0 0.0 0.0)  ; Fourth vertex (X, Y)
          (70 . 1)         ; Polyline flags (1 for closed polyline)
        ))
  (setq newPolyline (entmakex polylineData))
  (princ (strcat "\nCreated polyline: " (vl-princ-to-string newPolyline)))
  (princ)
)

(defun combine-lists (list1 list2)
  (if (null list1)
    list2
    (cons (car list1) (combine-lists list2 (cdr list1)))))

(defun create-polyline-with-bulges (vertices bulges)
  (setq polyline (entmakex
                   (setq ent (append (list
                     '(0 . "POLYLINE")
                     '(100 . "AcDbEntity")
                     '(100 . "AcDb2dPolyline")
                     '(66 . 1) ; Indicates optimized polyline format
                      (cons 90  (length vertices)) ; Number of vertices
                     '(70 . 1)) ; Flags: 1 for closed polyline, 0 for open
                      (combine-lists vertices bulges)
                   ))
                 )
               )
  polyline)

;; Usage example:
(defun c:tpl()
	(setq vertices '((0.0 0.0) (10.0 0.0) (10.0 10.0) (0.0 10.0))) ; Specify the vertices of the polyline
	(setq bulges '(0.0 0.5 0.0 0.5)) ; Specify the bulge values for each vertex
  (setq vertex-pairs (mapcar '(lambda (vertex) (cons 10 vertex)) vertices))
  (setq bulge-pairs (mapcar '(lambda (vertex) (cons 42 vertex)) bulges))
	(setq polyline (create-polyline-with-bulges vertex-pairs bulge-pairs))

	(princ "Polyline created with bulges: ")
	(princ polyline)
)



(defun dpl (points bulges)
  (entmake
    (list
      '(0 . "POLYLINE")	'(100 . "AcDbEntity") '(67 . 0) '(100 . "AcDb2dPolyline")
      '(66 . 1)	'(10 0.0 0.0 0.0) '(70 . 128) '(210 0.0 0.0 1.0))
  )
  (setq num-points (length points))
  (setq num-bulges (length bulges))

  (if (= num-points num-bulges)
    (progn
      (setq i 0)
      (repeat num-points
	(setq point (nth i points))
	(setq bulge (nth i bulges))
	(entmake
	  (list
	    '(0 . "VERTEX")
	    '(100 . "AcDbEntity")
	    '(100 . "AcDbVertex")
	    '(100 . "AcDb2dVertex")
	    (cons 10 point)
	    (cons 42 bulge)
	    '(70 . 0)
	  )
	)
	(setq i (+ i 1))
      )
      (entmake
	(list
	  '(0 . "SEQEND")
	  '(100 . "AcDbEntity")
	)
      )
    )
  )
  (princ)
)


(defun c:dph ()
  (setq p 0)
  (repeat '99 
    (DrawOffsetline (+ p 0.15) (+ p 14.85) 0.3)
    (DrawOffsetline (+ p 0.15) (+ p 14.85) -0.3)
    (DrawOffsetline (+ p 0.15) (+ p 14.85) 1.2)
    (DrawOffsetline (+ p 0.15) (+ p 14.85) -1.2)
    (setq p (+ p 15))
  )
)

(defun c:pa () ;polyline-area
  (setq pline (entsel "\nSelect a polyline: "))
  (if (and pline (setq pline (vlax-ename->vla-object (car pline))))
    (progn
      (setq area (vla-get-area pline))
      (princ (strcat "\nArea of the selected polyline: " (rtos area 2 6)))
    )
    (princ "\nInvalid selection. Please select a polyline.")
  )
  (princ)
)

;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue ( blk prp val )
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)

(defun c:setdynpropvalue()
(LM:setdynpropvalue (vlax-ename->vla-object (car (entsel))) "距離1" 200))

;(if (/= (getvar "acadver") "19.1s (LMS Tech)") (setvar "startmode" '0))

(defun create-block-reference ()
  (setq att-list '(((0 . "CHAINAGE")
                   (1 . "TT 0+456.000"))))
                   ;(0 . "ATTRIB2")
                   ;(1 . "Value2")))
  
  (setq att-ents '())
  (foreach att att-list
    (setq att-ents (cons (list (cons 0 "ATTRIB")
                               (cons 2 (cdar att))
                               (cons 1 (cdadr att)))
                         att-ents)))

  (setq blk-ent (list (cons 0 "INSERT")
                      (cons 2 "2")
                      (cons 10 '(0.0 0.0 0.0))
                      (cons 66 1)
                      (cons 70 0)
                      (cons 67 0)
                      (cons 410 "Model")))
  
  ;(setq blk-ent (append blk-ent att-ents))
  (entmake blk-ent)
  (entmake att-ents)
)

(defun c:cbar()
(create-block-reference))

;;; InsertBlockWithAttributes (Gilles Chanteau)
;;; Creates a new block reference with attributes using entmake
;;;
;;; Arguments
;;; blockName : name of the block definition
;;; inspt     : insertion point
;;; layer     : insertion layer
;;; xScale    : X scale
;;; yScale    : Y scale
;;; rotation  : rotation (radians)
;;; attribs   : list of dotted pairs containing the attribute values (TAG . Value)
(defun InsertBlockWithAttributes (blockName insPt layer xScale yScale rotation attribs / mxv block ent attDefs insert tag elst)
 
  (defun mxv (m v)
    (mapcar (function (lambda (r) (apply '+ (mapcar '* r v))))
            m
    )
  )
  
  (if (setq block (tblsearch "block" blockName))
    (progn
      (setq ent   (cdr (assoc -2 block))
            xform (list (list (* xScale (cos rotation)) (* xScale (- (sin rotation))) 0.)
                        (list (* yScale (sin rotation)) (* yScale (cos rotation)) 0.)
                        (list 0. 0. 1.)
                  )
      )
      (while ent
        (if (= "ATTDEF" (cdr (assoc 0 (setq elst (entget ent)))))
          (setq attDefs (cons (cons (cdr (assoc 2 elst)) elst) attDefs))
        )
        (setq ent (entnext ent))
      )
      (setq insert (entmakex
                     (list
                       (cons 0 "INSERT")
                       (cons 8 layer)
                       (cons 66 1)
                       (cons 2 blockName)
                       (cons 10 insPt)
                       (cons 41 xScale)
                       (cons 42 yScale)
                       (cons 43 1.0)
                       (cons 50 rotation)
                     )
                   )
      )
 
      (foreach att (reverse attDefs)
        (setq tag  (car att)
              elst (cdr att)
        )
        (entmakex
          (list
            (cons 0 "ATTRIB")
            (cons 100 "AcDbEntity")
            (assoc 8 elst)
            (cons 100 "AcDbText")
            (cons 10 (mapcar '+ inspt (mxv xform (cdr (assoc 10 elst)))))
            (cons 40 (* yScale (cdr (assoc 40 elst))))
            (cons 1
                  (cond ((cdr (assoc tag attribs)))
                        (T (cdr (assoc 1 elst)))
                  )
            )
            (cons 50 rotation)
            (cons 41 (/ xScale yscale))
            (assoc 51 elst)
            (assoc 7 elst)
            (assoc 72 elst)
            (cons 11 (mapcar '+ inspt (mxv xform (cdr (assoc 11 elst)))))
            (cons 100 "AcDbAttribute")
            (assoc 280 elst)
            (cons 2 tag)
            (assoc 70 elst)
            (assoc 74 elst)
            (assoc 280 (reverse elst))
          )
        )
      )
      (entmakex '((0 . "SEQEND")))
      (entlast)
    )
  )
)
