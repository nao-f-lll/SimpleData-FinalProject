;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space_invaders_final_project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1)
(define TANK-SPEED 2)  
(define MISSILE-SPEED 10) 

(define HIT-RANGE 10)
(define FIRE-RANGE 25)
(define INVADE-RATE 2)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#; 
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 WIDTH 1))
(define I4 (make-invader 170 40 1))             ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;;ListOfInvaders is one of:
;; - empty
;; - (list invaders ListOfInvaders)
;;Interp.  a list of invaders

(define LOI_1 empty)
(define LOI_2 (list I1 I2))

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))           ;not hit U1
(define M7 (make-missile 70 310)) 
(define M4 (make-missile 150 0)) 
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 15)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  10)))  ;> hit U1
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



;; ListOfMissiles is one of:
;; - empty
;; - (list Missile ListOfMissiles)
;; Interp. a list of missiles

(define LOM_1 empty)
(define LOM_2 (list M1 M2))

(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G5 (make-game (list I1) (list M3) T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game empty empty T2))

;; =================
;; Functions: 

;; game -> game
;; start the world with (main G0)

(define (main game)
  (big-bang game                     ; game
            (on-tick   updateGame)   ; game -> game
            (to-draw   renderGame)   ; game -> Image
            (stop-when gameOver overImage)     ; game -> Boolean    
            (on-key    handleKey)))  ; game KeyEvent -> game

;; game -> game
;; Update the state of the game
;; Move aliens, move missiles, hide destroyed aliens and missiles or beyond the limits of screen

#;
(check-random (updateGame G0)
              (make-game (list (make-invader (random WIDTH) (- HEIGHT HEIGHT) 1))
                         empty
                         (make-tank (+ (/ WIDTH 2) TANK-SPEED)1)))
#;
(check-random (updateGame G2)
              (make-game (list (make-invader (random WIDTH)(- HEIGHT HEIGHT) 1)
                               (make-invader (- 150 INVADER-Y-SPEED ) (- WIDTH INVADER-Y-SPEED) -1.5))
                         (make-missile 150 (- 300 MISSILE-SPEED))
                         (make-tank (+ 50 TANK-SPEED)1))) 
#;
(check-random (updateGame G5)
              (make-game (list (make-invader (random WIDTH)(- HEIGHT HEIGHT) 1)) empty (make-tank (+ 50 TANK-SPEED) 1)))

;;(define (updateGame game) game)

(define (updateGame game)
  (make-game (updateInvaders  game)
             (updateMissiles  game)
             (updateTank     (game-tank game))))


;; game -> ListOfInvaders
;; Update list of invaders based on game
;; Create and move all invaders first
;; then remove invaders which got shot
#;
(check-random  (updateInvaders (make-game (list (make-invader 200 40 -1.5)
                                                (make-invader 170 80 1.5)
                                                (make-invader (- 170 INVADER-X-SPEED) (- 50 INVADER-Y-SPEED) 1.5)
                                                (make-invader 300 20 1.5))
                                         (list (make-missile 170 60) (make-missile 70 310)) T1))

               (list (make-invader (random WIDTH) (- HEIGHT HEIGHT) 1.5)
                     (make-invader (- 200 INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED) -1.5)
                     (make-invader (+ 170 INVADER-X-SPEED) (+ 80 INVADER-Y-SPEED) 1.5)
                     (make-invader (- 300 INVADER-X-SPEED) (+ 20 INVADER-Y-SPEED) -1.5)))



;(define (updateInvaders game) (game-invaders game))

(define (updateInvaders game) 
  (filterInvaders (game-missiles game) (advanceInvaders (creatInvaders (game-invaders game)))))

;; ListOfInvaders -> ListOfInvaders
;; Create the invaders randomly on top of the screen if the random number is equal to INVADE-RATE

;;(check-random (creatInvaders empty) (list (make-invader (random WIDTH) (- HEIGHT HEIGHT) 1.5)))
#;
(check-random (creatInvaders (list (make-invader 200 40 -1.5)(make-invader 170 80 1.5)))

                             (list (make-invader (random WIDTH) (- HEIGHT HEIGHT) 1.5)
                                (make-invader 200 40 -1.5)(make-invader 170 80 1.5))
                             )

;;(define (creatInvaders loi)loi)

(define (creatInvaders loi)
   (if  (= INVADE-RATE  (random 20)) 
    (cons (make-invader (random WIDTH) (- HEIGHT HEIGHT) 1.5) loi)
     loi))


;; ListOfInvaders -> ListOfInvaders
;; moves the invaders on the x cord by add or sub 1 pixel and on the y add 1 pixels --
;; --  when invader reaches the border, change direction
(check-expect (advanceInvaders empty) empty)

(check-expect (advanceInvaders (list (make-invader 0 40 -1.5) (make-invader 200 40 -1.5)
                                     (make-invader 170 80 1.5) (make-invader 300 70 1.5)))

                                 (list (make-invader (+ 0 INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED) 1.5)
                                       (make-invader (- 200 INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED) -1.5)
                                       (make-invader (+ 170 INVADER-X-SPEED) (+ 80 INVADER-Y-SPEED) 1.5)
                                       (make-invader (- 300 INVADER-X-SPEED) (+ 70 INVADER-Y-SPEED) -1.5)
                                       ))

;;(define (advanceInvaders loi)loi)

(define (advanceInvaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advanceInvader (first loi))
              (advanceInvaders (rest loi)))]))

;; Invader -> Invader
;; move one invader by adding 1 or subtracting 1
(check-expect (advanceInvader (make-invader 200 40 -1.5)) (make-invader (- 200 INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED) -1.5))
(check-expect (advanceInvader (make-invader 0 40 -1.5))  (make-invader (+ 0 INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED)   1.5))
(check-expect (advanceInvader (make-invader 100 70 1.5)) (make-invader (+ 100 INVADER-X-SPEED) (+ 70 INVADER-Y-SPEED)  1.5))
(check-expect (advanceInvader (make-invader 300 60 1.5)) (make-invader (- 300 INVADER-X-SPEED) (+ 60 INVADER-Y-SPEED) -1.5))

;;(define (advanceInvader invader) invader)                

(define (advanceInvader invader)
  
  (if (checkInvaderDirectionAndWidth (invader-x invader) (invader-dx invader))

      (make-invader (+(invader-x invader)INVADER-X-SPEED) (+(invader-y invader)INVADER-Y-SPEED) 1.5)
      (make-invader (-(invader-x invader)INVADER-X-SPEED) (+(invader-y invader)INVADER-Y-SPEED) -1.5)
      ))

;; invader-x invader-dx -> Boolean
;; check if the coordinate and deriction of invader have to add 1 or to subtract 1
(check-expect (checkInvaderDirectionAndWidth 0 -1.5) true)
(check-expect (checkInvaderDirectionAndWidth 2 1.5) true)
(check-expect (checkInvaderDirectionAndWidth 300 -1.5) false)
(check-expect (checkInvaderDirectionAndWidth 300 1.5) false)
(check-expect (checkInvaderDirectionAndWidth 20 1.5) true)
(check-expect (checkInvaderDirectionAndWidth 20 -1.5) false)

;(define (checkInvaderDirectionAndWidth invader-x invader-dx) true)

(define (checkInvaderDirectionAndWidth invader-x invader-dx)
  
  (or (and (= invader-dx 1.5) (and (>= invader-x 0) (< invader-x 300)))
            (and (= invader-dx -1.5) (<= invader-x 0))))

;; ListOfMissiles ListOfInvaders -> ListOfInvaders
;; Filter the invaders and remove the ones that are hited by a missile
(check-expect (filterInvaders empty (list (make-invader 200 40 -1.5))) (list (make-invader 200 40 -1.5)))
(check-expect (filterInvaders empty empty) empty)

(check-expect (filterInvaders (list (make-missile 170 50) (make-missile 70 310))
                              (list (make-invader 200 40 -1.5) (make-invader 5 89 1.5)))

                             (list (make-invader 200 40 -1.5) (make-invader 5 89 1.5)))

(check-expect (filterInvaders (list (make-missile 170 50) (make-missile 170 60))
                              (list (make-invader 170 20 -1.5) (make-invader 170 50 1.5)))

                             (list (make-invader 170 20 -1.5)))

(check-expect (filterInvaders (list (make-missile 170 50) (make-missile 70 90))
                              (list (make-invader 200 50 -1.5) (make-invader 46 90 1.5)))

                             (list (make-invader 200 50 -1.5) (make-invader 46 90 1.5)))



;;(define (filterInvaders lom loi)loi)

(define (filterInvaders lom loi)
  (cond [(or (empty? loi) (empty? lom)) loi]
        [else
         (if (isInvadersHitByMissile (first loi) (inavderXcChecker (first loi) lom))
         (filterInvaders lom (rest loi))  
         (cons (first loi) (filterInvaders lom (rest loi))))]))


;;  Invader ListOfMissiles -> ListOfMissiles
;; return the missiles with the same x coordinate as the invader

(check-expect (inavderXcChecker (make-invader 170 90 1.5) empty ) empty)
                                
(check-expect (inavderXcChecker (make-invader 170 50 1.5) (list (make-missile 170 60) (make-missile 70 90)))
                                
              (list (make-missile 170 60)))

;(define (inavderXcChecker invader lom) lom)

(define (inavderXcChecker invader lom)
  (cond [(empty? lom) empty]
        [else
         (if (missileInvaderXC invader (first lom))
             (cons (first lom) (inavderXcChecker invader (rest lom))) 
             (inavderXcChecker invader (rest lom)))]))



;; Invader Missile -> Boolean
;; return true if the invader has the same x coordinate with one missile
(check-expect (missileInvaderXC (make-invader 200 50 1.5) (make-missile 200 78)) true)
(check-expect (missileInvaderXC (make-invader 75 60 1.5) (make-missile 10 60)) false)

;;(define (missileInvaderXC invader missile) true)

(define (missileInvaderXC invader missile)
        
             (<= (abs (- (missile-x missile)(invader-x invader))) HIT-RANGE))




;; Invader ListOfMissiles -> Boolean
;; Compare the Y coordinate of the invader and the missiles
(check-expect (isInvadersHitByMissile (make-invader 200 50 -1.5) empty) false)
(check-expect (isInvadersHitByMissile (make-invader 200 50 -1.5) (list (make-missile 200 89) (make-missile 70 70))) false)
(check-expect (isInvadersHitByMissile (make-invader 150 60 1.5) (list (make-missile 200 789) (make-missile 150 60))) true)
(check-expect (isInvadersHitByMissile (make-invader 150 50 1.5) (list (make-missile 200 789) (make-missile 150 60))) true)


;;(define (IsInvadersHitedByMissile invader lom) true)

(define (isInvadersHitByMissile invader lom)
  (cond [(empty? lom) false]
        [else
         (if (missileInvaderYC invader (first lom))
             true
              (isInvadersHitByMissile invader (rest lom)))]))

;; Invader Missile -> Boolean
;; Compare the Y coordinate of the invader and one missile
(check-expect (missileInvaderYC (make-invader 200 40 1.5) (make-missile 200 40)) true)
(check-expect (missileInvaderYC (make-invader 40 40 1.5) (make-missile 40 50)) true)
(check-expect (missileInvaderYC (make-invader 150 30 -1.5) (make-missile 150 60)) false)


;;(define (missileInvaderYC invader missile) true)

(define (missileInvaderYC invader missile)  
         
   (<= (abs (- (missile-y missile) (invader-y invader))) HIT-RANGE))           

;; game -> ListOfMissiles
;; Advance the missiles and filter them 

(check-expect (updateMissiles G0) empty)

(check-expect (updateMissiles (make-game (list (make-invader 200 40 1) (make-invader 170 40 1))
                                         (list (make-missile 170 50) (make-missile 70 310)) T1))

              (list (make-missile 70 (- 310 MISSILE-SPEED)))) 

(check-expect (updateMissiles G5) empty)
(check-expect (updateMissiles (make-game empty (list M4) T0)) empty)

;;(define (updateMissiles game) (game-missiles game))

(define (updateMissiles game)
  (passedHeight (hitInvader (game-invaders game) (advanceMissiles (game-missiles game)))))

;; ListOfMissiles -> ListOfMissiles
;; Check and remove the missiles that have been passed the MTS
(check-expect (passedHeight (list empty)) empty)

(check-expect (passedHeight (list (make-missile 150 300) (make-missile 150 0))) (list (make-missile 150 300) ))
(check-expect (passedHeight (list (make-missile 250 0))) empty )

;(define (passedHeight lom) lom)

(define (passedHeight lom)
  (cond [(or (empty? lom) (empty? (first lom))) empty]
        [else
         (if (checkHeightMissile (first lom))
         (passedHeight (rest lom))
         (cons (first lom) (passedHeight (rest lom))))]))

;; Missile -> Boolean
;; Return true if the missile passed the height
(check-expect (checkHeightMissile (make-missile 150 300)) false)
(check-expect (checkHeightMissile (make-missile 250 0)) true)

;(define (checkHeightMissile missile) true)

(define (checkHeightMissile missile)
   (<= (missile-y missile) 0))

;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; Check and remove the missiles when hiting the invaders
(check-expect (hitInvader empty empty) empty)
(check-expect (hitInvader empty (list (make-missile 150 300) (make-missile 50 20)))
              (list (make-missile 150 300) (make-missile 50 20)))

(check-expect (hitInvader (list (make-invader 90 15 -1) (make-invader 150 40 1))
                          (list (make-missile 250 300) (make-missile 50 15)))

                           (list (make-missile 250 300) (make-missile 50 15)))

(check-expect (hitInvader (list (make-invader 250 300 -1) (make-invader 150 40 1))
                          (list (make-missile 50 15) (make-missile 250 (- 310 HIT-RANGE))))

                           (list (make-missile 50 15)))

(check-expect (hitInvader (list  (make-invader 150 40 1) (make-invader 250 300 -1))
                          (list (make-missile 250 300) (make-missile 50 15) ))

                    (list (make-missile 50 15)))




;(define (hitInvader loi lom) lom)

(define (hitInvader loi lom)
  (cond [(or (empty? lom) (empty? loi)) lom]
        [else
         (if (IsMissilesHitingInvaders (first lom) (CheckCordinateX (first lom) loi)) 
        (hitInvader loi (rest lom))
        (cons (first lom) (hitInvader loi (rest lom))))]))



;; Missile ListOfInvaders  -> ListOfInvaders
;; Return the invaders that have the same x coordnate with missiles
(check-expect (CheckCordinateX (make-missile 50 15) empty) empty)

(check-expect (CheckCordinateX (make-missile 150 15) (list  (make-invader 20 40 1) (make-invader 150 300 -1)))
                               (list (make-invader 150 300 -1)))
            
(check-expect (CheckCordinateX (make-missile 90 15) (list  (make-invader 90 40 1) (make-invader 250 300 -1)))
                               (list (make-invader 90 40 1)))

(check-expect (CheckCordinateX (make-missile 90 15) (list  (make-invader 90 40 1) (make-invader 90 300 -1)))
                               (list  (make-invader 90 40 1) (make-invader 90 300 -1)))

(check-expect (CheckCordinateX (make-missile 190 15) (list  (make-invader 90 40 1) (make-invader 250 300 -1)))
                               empty)

;;(define (CheckCordinateX missile loi) loi)

(define (CheckCordinateX missile loi)
  (cond [(empty? loi) empty]
        [else
         (if (compareX missile (first loi))
             (cons (first loi) (CheckCordinateX missile (rest loi)))
              (CheckCordinateX missile (rest loi)))]))

;; Missile Invader  -> Boolean
;; Return true if the missile and the invader are in the smae x coordinate
;;(check-expect (compareX (make-missile 190 15) (make-invader 190 40 1)) true)
;;(check-expect (compareX (make-missile 20 15) (make-invader 190 40 1)) false)

;;(define (compareX missile invader) true)

(define (compareX missile invader)
  (<= (abs (- (missile-x missile) (invader-x invader))) HIT-RANGE))

;; Missile ListOfInvaders  -> Boolean
;; Return true if the missile hits the invader

(check-expect (IsMissilesHitingInvaders (make-missile 250 300) empty) false)

(check-expect (IsMissilesHitingInvaders (make-missile 250 40) (list (make-invader 250 300 -1) (make-invader 150 48 1)))
              false)

(check-expect (IsMissilesHitingInvaders (make-missile 250 300) (list (make-invader 250 300 -1) (make-invader 150 40 1)))
            true)



;;(define (IsMissilesHitingInvaders missile loi) true)

(define (IsMissilesHitingInvaders missile loi)
  (cond [(empty? loi) false]
        [else
         (if (invaderEqualMissile  missile (first loi)) 
             true
             (IsMissilesHitingInvaders missile (rest loi))
              )]))

;; Missile Invader  -> Boolean
;; Compare between the missile and the invader
(check-expect (invaderEqualMissile (make-missile 250 310) (make-invader 150 40 1)) false)
(check-expect (invaderEqualMissile (make-missile 150 160) (make-invader 150 140 -1)) false)
(check-expect (invaderEqualMissile (make-missile 150 160) (make-invader 150 160 -1)) true)

;;(define (invaderEqualMissile missile invader) false)

(define (invaderEqualMissile missile invader)
         (and
          (<= (abs (- (missile-x missile) (invader-x invader))) HIT-RANGE)
          (<= (abs (- (missile-y missile) (invader-y invader))) 3))) 

;; ListOfMissiles -> ListOfMissiles
;; Advance the missiles by MISSILE-SPEED pixels
(check-expect (advanceMissiles empty) empty)

(check-expect (advanceMissiles (list (make-missile 150 300) (make-missile 150 0)))
              (list (make-missile 150 (- 300 MISSILE-SPEED)) (make-missile 150 (- 0 MISSILE-SPEED))))  

;;(define (advanceMissiles lom) lom)

(define (advanceMissiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (advanceMissile (first lom))
              (advanceMissiles (rest lom)))]))

;; Missile -> Missile
;; advance the missile by 10 pixels
(check-expect (advanceMissile (make-missile 60 300)) (make-missile 60 (- 300 MISSILE-SPEED)))

;;(define (advanceMissile missile) missile)

(define (advanceMissile missile)
  (make-missile (missile-x missile) (- (missile-y missile) MISSILE-SPEED)))


;; tank -> tank
;; Advance the tank by TANK-SPEED pixels
(check-expect (updateTank (make-tank 120 1)) (make-tank (+ 120 TANK-SPEED) 1))
(check-expect (updateTank (make-tank 110 -1)) (make-tank (- 110 TANK-SPEED) -1))

;(define (updateTank tank) tank)

(define (updateTank t)
  (if (and (= (tank-dir t) 1) (< (tank-x t) WIDTH))
      (make-tank (+ (tank-x t)TANK-SPEED) (tank-dir t))
      (if (and (= (tank-dir t) 1) (> (tank-x t) WIDTH))
          (make-tank (- (tank-x t)TANK-SPEED) (-(tank-dir t)))
          (if (and (= (tank-dir t) -1) ( > (tank-x t) 0))
          (make-tank (- (tank-x t)TANK-SPEED) (tank-dir t))
          (make-tank (+ (tank-x t)TANK-SPEED) (-(tank-dir t)))
          )))) 

;; game -> Image
;; render game - tank, missiles, aliens
(check-expect (renderGame (make-game empty empty (make-tank (/ WIDTH 2) 1)))
       (place-image  empty-image 0 0 
       (place-image empty-image 0 0
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))


(check-expect (renderGame (make-game empty (list (make-missile 150 200) (make-missile 70 40)) (make-tank (/ WIDTH 2) 1)))

       (place-image  empty-image 0 0 
       (place-image MISSILE 150 200 (place-image MISSILE 70 40 
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))))

(check-expect (renderGame (make-game (list (make-invader 170 50 1.5) (make-invader 200 220 1.5))
                                     (list (make-missile 150 200) (make-missile 70 40))
                                     (make-tank (/ WIDTH 2) 1)))

       (place-image  INVADER 170 50 (place-image  INVADER 200 220 
       (place-image MISSILE 150 200 (place-image MISSILE 70 40 
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))

;;(define (renderGame game) BACKGROUND)


(define (renderGame game)
        (renderInvaders (game-invaders game)
        (renderMissiles (game-missiles game)
        (renderTank (game-tank game)))))

;; ListOfInvaders Image -> Image
;; render the the invaders
(check-expect (renderInvaders (list (make-invader 90 15 -1) (make-invader 150 40 1))
                              (place-image MISSILE 150 200 (place-image MISSILE 70 100 
                              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))

                              (place-image INVADER 90 15 (place-image INVADER 150 40
                               (place-image MISSILE 150 200 (place-image MISSILE 70 100 
                              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))

(check-expect (renderInvaders empty (place-image MISSILE 150 200 (place-image MISSILE 70 100 
                              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))

                              (place-image MISSILE 150 200 (place-image MISSILE 70 100 
                              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))
         )
                             
;;(define (renderInvaders loi image )INVADER)

(define (renderInvaders loi image)
  (cond [(empty? loi) image]
        [else
         (place-image INVADER (getXInvader (first loi)) (getYInvader (first loi))
              (renderInvaders (rest loi) image))]))


;; Invader -> Integer
;; get the x coordinate of the invader
(check-expect (getXInvader (make-invader 170 50 1.5)) 170)

;(define (getXInvader invader)0)

(define (getXInvader invader)
  (invader-x invader))

;; Invader -> Integer
;; get the y coordinate of the invader
(check-expect (getYInvader (make-invader 201 85 -1.5)) 85)

;;(define (getYInvader invader)0)

(define (getYInvader invader)
  (invader-y invader))

;; ListOfMissiles Image -> Image
;; render the the missiles
(check-expect (renderMissiles empty (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (renderMissiles (list (make-missile 150 200) (make-missile 70 100))
                              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
              
              (place-image MISSILE 150 200 (place-image MISSILE 70 100 
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))


;;(define (renderMissiles lom image)MISSILE)

(define (renderMissiles lom image)
  (cond [(empty? lom) image]
        [else
         (place-image MISSILE (returnXM (first lom)) (returnYM (first lom))
              (renderMissiles (rest lom) image))]))

;; Missile -> Integer
;; return the x coordinate of the missile
(check-expect (returnXM (make-missile 150 200)) 150)

;;(define (returnXM missile) 0)

(define (returnXM missile)
  (missile-x missile))

;; Missile -> Integer
;; return the y coordinate of the missile
(check-expect (returnYM (make-missile 150 200)) 200)

;;(define (returnYM missile) 0)

(define (returnYM missile)
  (missile-y missile))

;; Tank -> Image
;; render the the tank
(check-expect (renderTank (make-tank 50 -1)) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;;(define (renderTank tank )TANK)

(define (renderTank tank)
  (place-image TANK (tank-x tank) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; game -> Boolean 
;; decide if the game is over 
(check-expect (gameOver (make-game empty
                                   (list (make-missile 58 200) (make-missile 70 40))
                                   T0)) false)

(check-expect (gameOver (make-game (list (make-invader 170 50 1.5) (make-invader 200 (+ HEIGHT 1) 1.5))
                                   (list (make-missile 58 200) (make-missile 70 40))
                                   T0)) true)

(check-expect (gameOver (make-game (list (make-invader 170 50 1.5) (make-invader 200 78 1.5))
                                   (list (make-missile 58 200) (make-missile 70 40))
                                   T0)) false)

;;(define (gameOver game) false)

(define (gameOver game)
  (invadersLanded (game-invaders game)))

;; ListOfInvaders -> Boolean
;; Return true if any invader landed on Earth 
(check-expect (invadersLanded empty) false)

(check-expect (invadersLanded (list (make-invader 40 50 1.5) (make-invader 200 78 1.5))) false)
(check-expect (invadersLanded (list (make-invader 40 (+ HEIGHT 1) 1.5) (make-invader 200 78 1.5))) true)

;;(define (invadersLanded loi) true)

(define (invadersLanded loi)
  (cond [(empty? loi) false]
        [else
         (if (invaderYHeightCompare (first loi))
             true
              (invadersLanded (rest loi)))]))

;; Invader -> Boolean
;; Compare between the Y coordinate of the invader and the HEIGHT
(check-expect (invaderYHeightCompare (make-invader 40 50 1.5)) false)
(check-expect (invaderYHeightCompare (make-invader 200 (+ HEIGHT 1) -1.5)) true)

;;(define (invaderYHeightCompare invader) true)

(define (invaderYHeightCompare invader)
  (> (invader-y invader) HEIGHT))


;; Game -> Image
;; produces the game over screen

; (define (overImage game) BACKGROUND) ;stub

(define (overImage game)
  (place-image (text "GAME OVER" 50 "black") (/ WIDTH 2) (/ HEIGHT 2) (renderGame game)))


;; game KeyEvent -> game
;; Control the game when the left, right and space keys are pressed
(check-expect (handleKey G0  " ") (make-game empty (list (make-missile (/ WIDTH 2) (- HEIGHT FIRE-RANGE))) T0))
(check-expect (handleKey G3  " ") (make-game (list I1 I2) (list (make-missile 50 (- HEIGHT FIRE-RANGE )) M1 M2) T1))

(check-expect (handleKey G0  "left") (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handleKey G4  "right") (make-game empty empty (make-tank 50 1)))

(check-expect (handleKey G0  "c") G0)

(define (handleKey game ke)
  (cond [(key=? ke " ")     (make-game (game-invaders game) (fireMissiles(game-missiles game) (game-tank game))(game-tank game))]
        [(key=? ke "right") (make-game (game-invaders game) (game-missiles game) (controlTank (game-tank game) ke))]
        [(key=? ke "left")  (make-game (game-invaders game) (game-missiles game) (controlTank (game-tank game) ke))]
        [else 
         game]))


;; ListOfMissiles tank -> ListOfMissiles
;; Fire missiles
(check-expect (fireMissiles empty (make-tank 50 -1)) (list (make-missile 50 (- HEIGHT FIRE-RANGE ))))
(check-expect (fireMissiles (list M1 M2) (make-tank 150 1)) (list (make-missile 150 (- HEIGHT FIRE-RANGE)) M1 M2))

;;(define (fireMissiles lom tank) lom)

(define (fireMissiles lom tank)
  (cond [(empty? lom) (list (make-missile (tank-x tank) (- HEIGHT FIRE-RANGE)))]
        [else
         (cons (make-missile (tank-x tank) ( - HEIGHT FIRE-RANGE )) lom)]))

;; tank ke[right, left] -> tank
;; move the tank to right or to the left based on the key
(check-expect (controlTank (make-tank 50 -1) "right") (make-tank 50 1))
(check-expect (controlTank (make-tank 50 1) "left") (make-tank 50 -1))

;(define (controlTank tank ke) tank)

(define (controlTank tank ke)
  (if (key=? ke "right")
      (make-tank (tank-x tank) 1)
      (make-tank (tank-x tank) -1)))