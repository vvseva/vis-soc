extensions [ nw ]

globals [
  percent-similar  ;; on the average, what percent of a turtle's neighbors are the same color?
  percent-unhappy  ;; what percent of the turtles are unhappy?
  colors           ;; a list of colors we use to color the turtles

  inspected-turtle

  global-avg-link-dist  ;; The average distance of ALL turtles to their friends
  max-allowed-dist      ;; The current threshold distance (calculated from slider)
]

turtles-own [
  happy?              ;; is the turtle happy with its location?
  similar-nearby      ;; how many neighboring patches have a turtle with my color?
  other-nearby        ;; how many have a turtle of another color?
  total-nearby        ;; how many neighboring patches have a turtle?
  my-%-similar-wanted ;; the threshold for this particular turtle

  link-happy?         ;; Are my friends close enough?
  avg-link-dist       ;; The average distance to my network friends
]

to setup
  clear-all

  set inspected-turtle nobody
  set colors [red green yellow blue orange]

  ;; 1. GENERATE NETWORK (Social Structure)
  ;; Create turtles and link them in a ring lattice (Watts-Strogatz)
  nw:generate-watts-strogatz turtles links number average-degree rewiring-prob

  ;; 2. ASSIGN COLORS (Social Homophily)
  ;; Sort colors so neighbors in the network (ID 1, ID 2) share the same color.
  let color-pool []
  repeat number [
    set color-pool lput (item (random number-of-ethnicities) colors) color-pool
  ]
  set color-pool sort color-pool ;; <--- This sorting creates the network clustering

  let i 0
  foreach sort turtles [ t ->
    ask t [
      set color item i color-pool
      set shape "circle"
      set my-%-similar-wanted random %-similar-wanted
    ]
    set i i + 1
  ]

  ;; 3. RECOLOR LINKS
  recolor-links

  ;; 4. SCATTER TURTLES (Physical Randomness)
  ;; We create lists of turtles and patches and SHUFFLE them.
  ;; This breaks the spatial correlation, ensuring the model starts with
  ;; social clusters (links) that are physically scattered (high unhappiness).
  let turtle-list shuffle [self] of turtles
  let patch-list shuffle [self] of n-of count turtles patches

  (foreach turtle-list patch-list [ [t p] ->
    ask t [ move-to p ]
  ])

  update-turtles
  update-globals
  reset-ticks
end

to go
  if all? turtles [happy?] [ stop ]
  move-unhappy-turtles
  update-turtles
  update-globals

  manage-inspection

  tick
end

to move-unhappy-turtles
  ask turtles with [ not happy? ]
    [ find-new-spot ]
end

;; --- NEW: MODIFIED MOVEMENT LOGIC ---
to find-new-spot
  ;; 1. Identify valid spots around the turtle itself within the slider radius
  let local-candidates patches in-radius movement-radius with [ not any? turtles-here ]

  ;; 2. Identify valid spots around the turtle's network connections (link-neighbors)
  ;; We use 'patch-set' to combine the patches surrounding all link-neighbors into one agentset
  let network-candidates patch-set [ patches in-radius movement-radius ] of link-neighbors

  ;; Filter network candidates to ensure they are empty
  set network-candidates network-candidates with [ not any? turtles-here ]

  ;; 3. Combine both sets of candidates
  let all-candidates (patch-set local-candidates network-candidates)

  ;; 4. Move to one of them if available
  ifelse any? all-candidates [
    move-to one-of all-candidates
  ] [
    ;; Fallback: If no spots are found in the radius (overcrowding),
    ;; the turtle stays put and waits for a spot to open up next tick.
    ;; Alternatively, you can enable the line below to allow random jumps if stuck:
    ; move-to one-of patches with [not any? turtles-here]
  ]
end

to update-turtles
  ;; Pre-calculate world size for the formula
  ;; We use the world diagonal as the "Maximum Possible Distance"
  let world-diagonal sqrt (world-width ^ 2 + world-height ^ 2)

  ;; Calculate the specific distance threshold based on the slider
  ;; High % wanted = Low allowed distance
  let allowed-dist world-diagonal * (1 - (%-closeness-wanted / 100))

  ask turtles [
    ;; --- 1. SCHELLING (GRID) HAPPINESS ---
    set similar-nearby count (turtles-on neighbors) with [color = [color] of myself]
    set total-nearby count (turtles-on neighbors)
    set other-nearby count (turtles-on neighbors) with [color != [color] of myself]

    let grid-happy? (similar-nearby >= ( my-%-similar-wanted * total-nearby / 100 ))
                      and (other-nearby >= ( %-different-wanted * total-nearby / 100 ))

    ;; --- 2. NETWORK (LINK) HAPPINESS ---
    ;; Check if I have friends. If no friends, I am "link-happy" by default.
    ifelse any? link-neighbors [
      ;; Calculate average distance to all connected turtles
      ;; Note: 'distance' automatically finds the shortest path in the torus
      set avg-link-dist mean [distance myself] of link-neighbors

      ;; I am happy if my friends are closer than the allowed threshold
      set link-happy? (avg-link-dist <= allowed-dist)
    ] [
      set link-happy? true
    ]

    ;; --- 3. TOTAL HAPPINESS ---
    ;; Both conditions must be met to stop moving
    set happy? grid-happy? and link-happy?
  ]
end

to update-globals
  ;; 1. Existing Schelling stats
  let similar-neighbors sum [similar-nearby] of turtles
  let total-neighbors sum [total-nearby] of turtles

  ifelse total-neighbors > 0 [
    set percent-similar (similar-neighbors / total-neighbors) * 100
  ] [
    set percent-similar 0
  ]

  ifelse count turtles > 0 [
    set percent-unhappy (count turtles with [not happy?]) / (count turtles) * 100
  ] [
    set percent-unhappy 0
  ]

  ;; 2. NEW: Network Distance Stats
  ;; Calculate the threshold based on the current slider value
  let world-diagonal sqrt (world-width ^ 2 + world-height ^ 2)
  set max-allowed-dist world-diagonal * (1 - (%-closeness-wanted / 100))

  ;; Calculate the global average distance
  ;; We only count turtles that actually have friends (links)
  let linked-turtles turtles with [any? link-neighbors]

  ifelse any? linked-turtles [
    set global-avg-link-dist mean [avg-link-dist] of linked-turtles
  ] [
    set global-avg-link-dist 0
  ]
end


to toggle-links
  ifelse any? links with [ not hidden? ] [
    ask links [ hide-link ]
  ] [
    ask links [ show-link ]
  ]
  display ;; Forces the view to update immediately
end


to recolor-links
  ask links [
    ;; Check the color of the two turtles at the ends of the link
    let c1 [color] of end1
    let c2 [color] of end2

    ifelse c1 = c2 [
      ;; If same color, use that color
      set color c1
      ;; Optional: Make same-color links slightly thicker to see them better
      set thickness 0
    ] [
      ;; If different, use gray
      set color gray
      set thickness 0  ;; default thickness
    ]
  ]
end


to manage-inspection
  ifelse inspect-switch [
    ;; --- SWITCH IS ON ---

    ;; 1. If we aren't watching anyone yet, pick a random turtle
    if inspected-turtle = nobody [
      set inspected-turtle one-of turtles
      watch inspected-turtle
    ]

    ;; 2. Draw the visualization (Local & Network)
    draw-inspection

  ] [
    ;; --- SWITCH IS OFF ---

    ;; If we were watching someone, clear everything and stop
    if inspected-turtle != nobody [
      reset-inspection
    ]
  ]
end

to draw-inspection
  ;; Clear the board first so we don't leave trails
  ask patches [ set pcolor black ]

  if inspected-turtle != nobody [
    ask inspected-turtle [
      set size 3
      type "Avg Distance to Friends: " print precision avg-link-dist 2
      ;; A. NETWORK RADIUS (Friends' Neighborhoods) -> LIGHT GREEN
      ;; We draw this first so the local radius (Blue) draws on top if they overlap
      ask link-neighbors [
        ask patches in-radius movement-radius [
          set pcolor lime ;; Light Green
        ]
      ]

      ;; B. LOCAL RADIUS (My Neighborhood) -> LIGHT BLUE
      ask patches in-radius movement-radius [
        set pcolor sky  ;; Light Blue
      ]

      ;; C. SHOW LINKS
      ask my-links [ show-link set thickness 0.5 ]
    ]
  ]
end

to reset-inspection
  set inspected-turtle nobody
  reset-perspective        ;; Stops camera tracking
  ask turtles [ set size 1 ]
  ask links [ hide-link ]
  ask patches [ set pcolor black ]
end
@#$#@#$#@
GRAPHICS-WINDOW
273
10
638
376
-1
-1
7.0
1
10
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
30.0

MONITOR
465
410
578
455
Percent Unhappy
percent-unhappy
1
1
11

MONITOR
341
410
449
455
Percent Similar
percent-similar
1
1
11

PLOT
10
180
259
323
Percent Similar
time
%
0.0
25.0
0.0
100.0
true
false
"" ""
PENS
"percent" 1.0 0 -2674135 true "" "plot percent-similar"

PLOT
10
324
259
488
Percent Unhappy
time
%
0.0
25.0
0.0
100.0
true
false
"" ""
PENS
"percent" 1.0 0 -10899396 true "" "plot percent-unhappy"

SLIDER
15
55
240
88
number
number
500
2500
1870.0
10
1
NIL
HORIZONTAL

SLIDER
15
95
240
128
%-similar-wanted
%-similar-wanted
0.0
100.0
33.0
1.0
1
%
HORIZONTAL

BUTTON
34
14
114
47
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
124
14
204
47
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
35
500
227
533
number-of-ethnicities
number-of-ethnicities
2
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
15
125
240
158
%-different-wanted
%-different-wanted
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
275
465
467
498
movement-radius
movement-radius
0
50
4.0
1
1
NIL
HORIZONTAL

SLIDER
275
510
447
543
average-degree
average-degree
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
465
520
637
553
rewiring-prob
rewiring-prob
0
1
0.4
0.1
1
NIL
HORIZONTAL

BUTTON
500
470
622
503
NIL
toggle-links
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
620
415
797
448
inspect-switch
inspect-switch
0
1
-1000

BUTTON
210
15
265
48
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
15
155
240
188
%-closeness-wanted
%-closeness-wanted
0
100
78.0
1
1
%
HORIZONTAL

PLOT
665
20
960
230
Network Distances
ticks
Distance
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Allowed Max" 1.0 0 -2139308 true "" "plot max-allowed-dist"
"Current Avg" 1.0 0 -7500403 true "" "plot global-avg-link-dist"

@#$#@#$#@
## ACKNOWLEDGMENT

This model is from Chapter Three of the book "Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo", by Uri Wilensky & William Rand.

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

This model is in the IABM Textbook folder of the NetLogo Models Library. The model, as well as any updates to the model, can also be found on the textbook website: http://www.intro-to-abm.com/.

## WHAT IS IT?

This project models the behavior of two types of turtles in a mythical pond. The red turtles and green turtles get along with one another. But each turtle wants to make sure that it lives near some of "its own." That is, each red turtle wants to live near at least some red turtles, and each green turtle wants to live near at least some green turtles. The simulation shows how these individual preferences ripple through the pond, leading to large-scale patterns. This model is an extension of the Segregation Simple Extension 2 model. It adds a constraint that agents also want to have a minimum number of different agents around them.

This project was inspired by Thomas Schelling's writings about social systems (particularly with regards to housing segregation in cities).

## HOW TO USE IT

Click the SETUP button to set up the turtles. There are equal numbers of each color turtles. The turtles move around until there is at most one turtle on a patch.  Click GO to start the simulation. If turtles don't have enough same-color neighbors, they jump to a nearby patch.

The NUMBER slider controls the total number of turtles. (It takes effect the next time you click SETUP.) The NUMBER-OF-ETHNICITIES slider controls the number of different types of turtles, each a different color. The %-SIMILAR-WANTED slider controls the percentage of same-color turtles that each turtle wants among its neighbors. For example, if the slider is set at 30, each green turtle wants at least 30% of its neighbors to be green turtles. The %-DIFFERENT-WANTED slider controls the percentage of different-color turtles that each turtle wants among its neighbors. Turtles can only be happy if both the %-similar and %-different constraints are met.

The "PERCENT SIMILAR" monitor shows the average percentage of same-color neighbors for each turtle. It starts at about 0.5, since each turtle starts (on average) with an equal number of red and green turtles as neighbors. The "PERCENT UNHAPPY" monitor shows the percent of turtles that have fewer same-ethnicity neighbors than they want (and thus want to move).  Both monitors are also plotted.

## THINGS TO NOTICE

When you execute SETUP, the turtles are randomly distributed throughout the pond. But many turtles are "unhappy" since they don't have enough neighbors of the same ethnicity. The unhappy turtles jump to new locations in the vicinity. But in the new locations, they might tip the balance of the local population, prompting other turtles to leave. If a few red turtles move into an area, the local blue or orange turtles might leave. But when the blue or orange turtles move to a new area, they might prompt red turtles to leave that area, and so on.

Over time, the number of unhappy turtles decreases. But the pond becomes more segregated, with clusters of each ethnicity.

Again, relatively small individual preferences can lead to significant overall segregation. The exact numbers depend on how many ethnicities you have, and on the random distribution of their preferences for similarity.

## THINGS TO TRY

How does the diversity in PERCENT-SIMILAR-WANTED for each ethnicity affect the overall segregation pattern?

How does the added constraint of the PERCENT-DIFFERENT-WANTED for each ethnicity affect the overall segregation pattern?

Try different values for %-SIMILAR-WANTED. How does the overall degree of segregation change?

Try different values for NUMBER-OF-ETHNICITIES. How does the overall segregation pattern change?

If each turtle wants at least 40% same-color neighbors, what percentage (on average) do they end up with?

For what slider settings does the model run forever without all agents being satisfied?

## NETLOGO FEATURES

In the UPDATE-GLOBALS procedure, note the use of SUM, COUNT, VALUES-FROM, and WITH to compute the percentages displayed in the monitors and plots.

## CREDITS AND REFERENCES

This model is a simplified version of:

* Wilensky, U. (1997).  NetLogo Segregation model.  http://ccl.northwestern.edu/netlogo/models/Segregation.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Schelling, T. (1978). Micromotives and Macrobehavior. New York: Norton.

See also: Rauch, J. (2002). Seeing Around Corners; The Atlantic Monthly; April 2002;Volume 289, No. 4; 35-48. https://www.theatlantic.com/magazine/archive/2002/04/seeing-around-corners/302471/

## HOW TO CITE

This model is part of the textbook, “Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo.”

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U., Rand, W. (2006).  NetLogo Segregation Simple Extension 3 model.  http://ccl.northwestern.edu/netlogo/models/SegregationSimpleExtension3.  Center for Connected Learning and Computer-Based Modeling, Northwestern Institute on Complex Systems, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the textbook as:

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

## COPYRIGHT AND LICENSE

Copyright 2006 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2006 Cite: Wilensky, U., Rand, W. -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
