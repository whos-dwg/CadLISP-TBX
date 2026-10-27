(defun TBX:InsertBlockWithAttributes (space ins_pt block_path / obj ss)
  ;; ATTREQ = 0: use all default attribute values.
  ;; ATTREQ = 1, ATTDIA = 0: use command-line attribute prompts.
  ;; ATTREQ = 1, ATTDIA = 1: show the attribute dialog box.
  (cond
    ((= (getvar "ATTREQ") 0)
      (vla-InsertBlock space (vlax-3d-point ins_pt) block_path 1.0 1.0 1.0 0.0)
    )

    ((= (getvar "ATTDIA") 0)
      (command "_.-INSERT" block_path ins_pt 1.0 1.0 0.0)
      (if (setq ss (ssget "_L" '((0 . "INSERT"))))
        (vlax-ename->vla-object (ssname ss 0))
      )
    )

    (t
      (setq obj
        (vla-InsertBlock space (vlax-3d-point ins_pt) block_path 1.0 1.0 1.0 0.0)
      )
      (if (= (vla-get-HasAttributes obj) :vlax-true)
        (command "_.DDATTE" (vlax-vla-object->ename obj))
      )
      obj
    )
  )
)

(defun C:TBX (/ blk_name frame_name w p1 p2 minX maxX minY maxY dx dy tag_pt close_pt doc pspace frame_obj props prop_name blk_obj frame_path tag_path old_attreq old_attdia minpt maxpt bmin bmax bminX bminY bmaxX bmaxY temp_pts temp_pl pts line_obj tag_obj ltypes ltype_obj old_osmode aa count g raw_prop_name lin_path)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq pspace (vla-get-PaperSpace doc))

  ;; --- 1. SETTING ---
  (setq blk_name "blowup tag" 
        frame_name "FRAME" 
        w 0.5)

  (setq old_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; --- 修正：正確載入 acadiso.lin 的標準 HIDDEN 線型 ---
  (setq ltypes (vla-get-Linetypes doc))
  (if (not (tblsearch "LTYPE" "HIDDEN"))
    (progn
      ;; 尋找系統中的 acadiso.lin 路徑
      (setq lin_path (findfile "acadiso.lin"))
      (if lin_path
        ;; 如果找到了 acadiso.lin，則從該檔案精準載入 "HIDDEN" 線型
        (vl-catch-all-apply 'vla-Load (list ltypes "HIDDEN" lin_path))
        ;; 萬一完全找不到該檔（極端情況），才做基礎的後備建立
        (progn
          (setq ltype_obj (vla-add ltypes "HIDDEN"))
          (vla-put-Description ltype_obj "Hidden __ __ __ _")
        )
      )
    )
  )

  ;; --- 2. SEARCH BLOCK ---
  (setq frame_path (cond 
    ((tblsearch "BLOCK" frame_name) frame_name)
    ((findfile (strcat frame_name ".dwg")) (strcat frame_name ".dwg"))
    (t nil)
  ))

  (setq tag_path (cond 
    ((tblsearch "BLOCK" blk_name) blk_name)
    ((findfile (strcat blk_name ".dwg")) (strcat blk_name ".dwg"))
    (t nil)
  ))

  (if (not frame_path)
    (progn 
      (setvar "OSMODE" old_osmode)
      (alert "Error: BLOCK [FRAME] not found!")
      (exit)
    )
  )

  (if (not tag_path)
    (progn 
      (setvar "OSMODE" old_osmode)
      (alert "Error: BLOCK [blowup tag] not found!")
      (exit)
    )
  )

  ;; --- 3. GET POINT ---
  (setvar "OSMODE" old_osmode)
  (setq p1 (getpoint "\nPick first corner: "))
  (if (not p1) (exit))
  (setq p2 (getcorner p1 "\nPick opposite corner: "))
  (if (not p2) (exit))
  (setvar "OSMODE" 0)

  (setq minX (min (car p1) (car p2))
        maxX (max (car p1) (car p2))
        minY (min (cadr p1) (cadr p2))
        maxY (max (cadr p1) (cadr p2))
        dx (abs (- maxX minX))
        dy (abs (- maxY minY)))

  ;; --- 4. INSERT FRAME ---
  (setq frame_obj (vla-InsertBlock pspace (vlax-3d-point (list minX minY 0)) frame_path 1 1 1 0))
  
  (setq props (vlax-invoke frame_obj 'GetDynamicBlockProperties))
  (setq count 1) 
  
  (foreach prop props
    (setq raw_prop_name (vla-get-PropertyName prop))
    (setq prop_name (strcase raw_prop_name))
    
    (if (or (wcmatch raw_prop_name "*寬*")
            (wcmatch prop_name "*WID*")
            (wcmatch raw_prop_name "*距離1*")
            (wcmatch prop_name "*DIST1*"))
        (vl-catch-all-apply 'vla-put-Value (list prop (vlax-make-variant (float dx) 5)))
    )

    (if (or (wcmatch raw_prop_name "*高*")
            (wcmatch prop_name "*HEI*")
            (wcmatch raw_prop_name "*距離2*")
            (wcmatch prop_name "*DIST2*"))
        (vl-catch-all-apply 'vla-put-Value (list prop (vlax-make-variant (float dy) 5)))
    )

    (if (wcmatch prop_name "*PARAM*")
        (progn
          (if (= count 1)
              (progn
                (vl-catch-all-apply 'vla-put-Value (list prop (vlax-make-variant (float dx) 5)))
                (setq count 2)
              )
              (progn
                (vl-catch-all-apply 'vla-put-Value (list prop (vlax-make-variant (float dy) 5)))
                (setq count 1)
              )
          )
        )
    )
  )
  (vla-update frame_obj)

  ;; --- 5. DRAW LINE ---
  (setvar "OSMODE" old_osmode)
  (setq tag_pt (getpoint "\nPick tag position: "))
  (setvar "OSMODE" 0)
  
  (if tag_pt
    (progn
      (vla-GetBoundingBox frame_obj 'minpt 'maxpt)
      (setq bmin (vlax-safearray->list minpt)
            bmax (vlax-safearray->list maxpt))
      (setq bminX (car bmin)
            bminY (cadr bmin)
            bmaxX (car bmax)
            bmaxY (cadr bmax))

      (setq temp_pts (vlax-make-safearray vlax-vbDouble '(0 . 7)))
      (vlax-safearray-fill temp_pts (list bminX bminY bmaxX bminY bmaxX bmaxY bminX bmaxY))
      (setq temp_pl (vla-AddLightWeightPolyline pspace temp_pts))
      (vla-put-Closed temp_pl :vlax-true)
      
      (setq close_pt (vlax-curve-getClosestPointTo temp_pl tag_pt))
      (vla-Delete temp_pl) 

      (setq pts (vlax-make-safearray vlax-vbDouble '(0 . 3)))
      (vlax-safearray-fill pts (list (car close_pt) (cadr close_pt) (car tag_pt) (cadr tag_pt)))
      (setq line_obj (vla-AddLightWeightPolyline pspace pts))
      (vla-put-ConstantWidth line_obj w)
      
      ;; 套用載入好的 HIDDEN 線型
      (if (tblsearch "LTYPE" "HIDDEN")
        (vla-put-Linetype line_obj "HIDDEN")
        (vla-put-Linetype line_obj "Continuous")
      )
      
      ;; 提示：配合公制(acadiso)的 HIDDEN 性質，可視需要調整此處的 LinetypeScale (原本為0.5)
      (vla-put-LinetypeScale line_obj 0.5)
      (vla-put-Layer line_obj (getvar "CLAYER"))

      ;; --- 6. INSERT TAG ---
      ;; Follow the current ATTREQ / ATTDIA system-variable settings.
      (setvar "OSMODE" old_osmode)
      (setq tag_obj (TBX:InsertBlockWithAttributes pspace tag_pt tag_path))
      (setvar "OSMODE" 0)

      (if tag_obj
        (vla-update tag_obj)
      )
    )
  )
  (setvar "OSMODE" old_osmode)
  (princ "\nDone.")
  (princ)
)