;; Modelling the willingness to donate organs ;;
;; A project work by X, Y and N*****s Re****** ;;
;; Module: Behavioural Public Administration ;;
;; University of X ;;






;; Own contribution

turtles-own
[
  favorable?           ;; turtle is favorable to donation
  unfavorable?         ;; turtle is unfavorable to donation
  christian?           ;; turtle has strong Christian values
  non-christian?       ;; turtle has not strong Christian values
]



;;;;;;;;;;;;;;;;;;
;;; SETUP PART ;;;
;;;;;;;;;;;;;;;;;;


;; First, the spatially-clustered network
  ;; Initial code taken from 'Virus on a Network model' but strongly modified
to setup-spatially-clustered-network
  clear-all
  setup-nodes
  setup-spatial-network
  ask n-of people-willing-to-donate turtles
    [ become-favorable]
  ask turtles [
    if favorable? = 0 [
      become-unfavorable]]
  ask n-of number-of-christians turtles
    [ become-christian]
  ask turtles [
    if christian? = 0 [
      become-non-christian]
      set size 1.5]
  ask links [ set color grey ]
  ask patches [ set pcolor white ]
  reset-ticks
end


;; Secondly, the simple-random-network
  ;; Initial code taken from 'Random Network model' but strongly modified
to setup-simple-random-network
  clear-all
  setup-nodes
  setup-simple-random
  ask n-of people-willing-to-donate turtles
    [ become-favorable]
  ask turtles [
    if favorable? = 0 [
      become-unfavorable]]
  ask n-of number-of-christians turtles
    [ become-christian]
  ask turtles [
    if christian? = 0 [
      become-non-christian]
      set size 1.5]
  ask links [ set color grey ]
  ask patches [ set pcolor white ]
  reset-ticks
end


;; Finally, the small-world network
  ;; Initial code taken from 'Small worlds models' but strongly modified
to setup-small-world
  clear-all
  ; make the nodes and arrange them in a circle in order by who number
  setup-nodes
  layout-circle (sort turtles) max-pxcor - 1
  ; Create the initial lattice
  wire-lattice
  ask n-of people-willing-to-donate turtles
    [ become-favorable]
  ask turtles [
    if favorable? = 0 [
      become-unfavorable]]
  ask n-of number-of-christians turtles
    [ become-christian]
  ask turtles [
    if christian? = 0 [
      become-non-christian]
      set size 1.5]
  ask links [ set color grey ]
  ask patches [ set pcolor white ]
  reset-ticks
end


;; Taken from 'Virus on a Network' and adapted to our model
to setup-nodes
  set-default-shape turtles "person"
  create-turtles number-of-people
  ; for visual reasons, we don't put any nodes *too* close to the edges
  [ setxy (random-xcor * 0.95) (random-ycor * 0.95) ]
  end




;; COMMAND CREATION SPATIALLY CLUSTERED NETWORK ;;

  ;; Taken from 'Virus on a Network model' and not modified
to setup-spatial-network
  let num-links-2 (average-node-degree * number-of-people) / 2
  while [count links < num-links-2 ]
  [
    ask one-of turtles
    [
      let choice (min-one-of (other turtles with [not link-neighbor? myself])
                   [distance myself])
      if choice != nobody [ create-link-with choice ]
    ]
  ]
  ; make the network look a little prettier
  repeat 10
  [
    layout-spring turtles links 0.3 (world-width / (sqrt number-of-people)) 1
  ]
end

;; COMMAND CREATION SIMPLE-RANDOM-NETWORK ;;

  ;; Taken from 'Random Network model'
to setup-simple-random
    if num-links-simple-random-network > max-links [ set num-links-simple-random-network max-links ]
  while [count links < num-links-simple-random-network] [
    ask one-of turtles [ create-link-with one-of other turtles ]
    ]
end

to-report max-links
  ;; report the maximum number of links that can be added
  ;; to a random network, given the specified number
  ;; of nodes, with 1000 as an arbitrary upper bound
  report min (list (number-of-people * (number-of-people - 1) / 2) 1000)
end



;; COMMAND CREATION SMALL WORLD ;;

  ;; Taken from 'Small worlds models'
; creates a new lattice
to wire-lattice
  ; iterate over the turtles
  let n 0
  while [ n < count turtles ] [
    ; make edges with the next two neighbors
    ; this makes a lattice with average degree of 4
    make-edge turtle n
              turtle ((n + 1) mod count turtles)
              "default"
    ; Make the neighbor's neighbor links curved
    make-edge turtle n
              turtle ((n + 2) mod count turtles)
              "curve"
    set n n + 1
  ]

  ; Because of the way NetLogo draws curved links between turtles of ascending
  ; `who` number, two of the links near the top of the network will appear
  ; flipped by default. To avoid this, we used an inverse curved link shape
  ; ("curve-a") which makes all of the curves face the same direction.
  ask link 0 (count turtles - 2) [ set shape "curve-a" ]
  ask link 1 (count turtles - 1) [ set shape "curve-a" ]
end

; Connects two nodes
to make-edge [ node-A node-B the-shape ]
  ask node-A [
    create-link-with node-B  [
      set shape the-shape

    ]
  ]
end









;;;;;;;;;;;;;;;
;;; GO PART ;;;
;;;;;;;;;;;;;;;

;; Own contribution


to go
  if all? turtles [unfavorable?] [
    stop]
  spread-willingness-to-christians
  spread-willingness-to-non-christians
  if ticks >= 8000 [ stop ]
  tick
end


to spread-willingness-to-christians
  ask turtles with [favorable?]
      [ask link-neighbors with [unfavorable? AND non-christian?]
        [ if random-float 10000 < ease-to-transmit
            [ become-favorable ]]]
end

to spread-willingness-to-non-christians
  ask turtles with [favorable?]
      [ask link-neighbors with [unfavorable? AND christian?]
        [ if random-float 10000000 < ease-to-transmit
            [ become-favorable ]]]
end





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHANGE OF STATE VARIABLES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Own contribution


to become-favorable ;; turtle procedure
  set favorable? True
  set unfavorable? False
  set color green
end


to become-unfavorable  ;; turtle procedure
  set favorable? False
  set unfavorable? True
  set color red
end


to become-christian ;; turtle procedure
  set christian? True
  set non-christian? False
end


to become-non-christian  ;; turtle procedure
  set christian? False
  set non-christian? True
end
