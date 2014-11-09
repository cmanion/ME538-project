globals[
  solarcellpower
  producerpowerneededperstep
  workerbatterycapacity
  workerchargetomove ;;should eventually be proportional to how much it is carrying
  workerregolithcapacity ;; amount of regolith a worker can hold
  productcostinformation ;;information on how much a product cost to make for a producer
  producerregolithcapacity ;;amount of regolith a producer can store
  producerproductcapacity ;;amount of finished products a producer can store
  
  ]

breed [solarcells solarcell]
breed [producers producer]
breed [workers worker]
breed [launchers launcher]
patches-own[
  cluster 
  regolith
  paver?
  radio-a
  poweravailable
  ]

producers-own[
  pregolith
  turnsleft
  productid
  capacity
  productstack
  ]
workers-own[
  wregolith
  wcharge
  wheld
  ]
to setup
clear-all
setglobals
setup-patches
set-default-shape solarcells "die 6"
set-default-shape producers "factory"
;;set-default-shape producers "square"
;;set-default-shape workers "default"
set-default-shape workers "bulldozer top"
set-default-shape launchers "container"
setup-seed 0 0
recolor-all

end

to go
 addpower
 ask producers with [not hidden?]
 [
   produceproduct
 ]
 ask patches [displaypoweravailable]
end

to setglobals
  set solarcellpower 1
  set producerproductcapacity 4 ;; producers can store 4 solar cells or pavers, or 1 of anything else
  set productcostinformation
  (list
    ;; power_cost regolith_cost turns capacity_cost
    (list 1 1 3 1);;paver cost
    (list 1 1 5 1);;solar cell cost
    (list 1 2 10 4);;worker cost, workers drive off cell
    (list 1 5 20 4);;producer cost
    (list 1 6 25 4);;launcher cost
  )
end

to setup-patches
  ask patches
  [
    setup-regolith
    recolor-patches
  ]
  
end

to setup-regolith
  set cluster nobody
  set regolith random-float 10
  set paver? false
  set poweravailable 0
end

to recolor-all
  ask patches
  [recolor-patches]
end

to setup-seed[ x y]

  ask patch x y
  [
    set paver? true
    ask neighbors [set paver? true]
    ask neighbors4 [sprout-solarcells 1 [set color blue]]

  ]
  create-producers 1 
  [
    
    setxy x y
    initializeproducer
    
  ]
  create-workers 1
  [
   setxy x y
   set heading 0
   initializeworker
  
  ]
    
end

to produceproduct
  ;;function for the producer to produce stuff
  ;; power_cost regolith_cost turns capacity_cost
 if productid > -1 [
   let costvector item productid productcostinformation
   let powerrequired item 0 costvector
   let totregolith item 1 costvector
   let turns item 2 costvector
   let capacitycost item 3 costvector
 
   let regolithperturn (totregolith / turns)
   if turnsleft > 0
   [
     if pregolith > regolithperturn ;; if there is enough regolith to make product
     [
      
       if usepower powerrequired ;;if there is enough power
       [
         set pregolith pregolith - regolithperturn
         set turnsleft turnsleft - 1
       ]
     ]
  
   ]
   if turnsleft = 0
   [
     if productid = 0
     [
       set productstack lput 1 productstack ;put paver on the stack of products made
       set productid -1
       set capacity capacity - capacitycost ;decrease the amount of storage space remaining 
     ]
     if productid = 1
     [
     hatch-solarcells 1 ;;this is a bit of a complicated function here, calling myself is calling the producer and telling it to added the solar cell(self) it created to the product stack
     [
       initializesolarcell
       ht; hide
       let producttoadd self
       ask myself 
       [
         
         set productstack lput producttoadd productstack ;;add
         
       ]
     ]
     set productid -1
     set capacity capacity - capacitycost  
     ]
     if productid = 3
     [
     hatch-producers 1 
     [
       initializeproducer
       ht; hide
       let producttoadd self
       ask myself 
       [
         
         set productstack lput producttoadd productstack ;;add
         
       ]
     ]
     set productid -1
     set capacity capacity - capacitycost  
     ]
     if productid = 4
     [
     hatch-launchers 1 ;;this is a bit of a complicated function here, calling myself is calling the producer and telling it to added the solar cell(self) it created to the product stack
     [
       initializelauncher
       ht; hide
       let producttoadd self
       ask myself 
       [
         
         set productstack lput producttoadd productstack ;;add
         
       ]
     ]
     set productid -1
     set capacity capacity - capacitycost  
     ]
     if productid = 2 ;making a worker, workers automatically drive off
     [
       hatch-workers 1
       [
         initializeworker
         fd 1 
       ]
      set productid -1
      ;;workers have no capacity cost because they drive off
     ]      
   ]
  ]
end

to tellproducerstomakepaver
  ask producers with [not hidden?]
  [startproducingproduct 0]

end

to tellproducerstomakesolarcell
  ask producers with [not hidden?]
  [startproducingproduct 1]

end

to startproducingproduct[toproduce]
   ;;start producing a product
   ;;might make this a bool
   
   if (toproduce > -1) and (productid = -1 )[ ;; if we tell the producer to produce something and it is not producing anything
      let costvector item toproduce productcostinformation
      let powerrequired item 0 costvector
      let totregolith item 1 costvector
      let turns item 2 costvector
      let regolithperturn (totregolith / turns)
      let capacitycost item 3 costvector
      
      if (capacity - capacitycost) >= 0; if there is enough space to strore an output product
      [
        ;if pregolith > regolithperturn ; if there is enough regolith to make the product honestly we don't need to do this
        ;[
          ;if powerrequired > poweravailable ; if there is enough power
          ;[;
            set turnsleft turns
            set productid toproduce
          ;]
        ;]
      ]
   ]
   
end
to-report usepower [amount]
  if paver?
  [
    if poweravailable > amount
    [
      let currentcluster cluster
      ask patches with [cluster = currentcluster]
      [
        set poweravailable poweravailable - amount
        
      ]
      report true
    ]
  ]
  report false
end

to initializeproducer
    set heading 0
    set color green
    set productid -1
    set productstack []
    set capacity producerproductcapacity
    set pregolith 20; REMOVE THIS FOR DEBUGGING ONLY!
end

to initializeworker
  
  set color red
end

to initializesolarcell
  set color blue
end

to initializelauncher
  set color cyan
  ;eventually add more stuff here as we make it more complicated
end

to grow-cluster  ;; patch procedure
  ask neighbors4 with [(cluster = nobody) and (paver? = true)]
  [ set cluster [cluster] of myself
    grow-cluster ]
 
end

to addpower
  ask patches 
  [
    set poweravailable 0
    
  ]
  find-clusters
  ask solarcells with[not hidden?]
  [
    solarcelladdpower
  ]
  
end

to solarcelladdpower
  if paver?
  [
    let currentcluster cluster
    ask patches with [cluster = currentcluster]
    [
      set poweravailable poweravailable + solarcellpower
    ]
  ]
end

to displaypoweravailable ;;patch function
  ifelse paver?[
    set plabel poweravailable
  ]
  [
    set plabel ""
  ]
end
to recolor-patches
  ifelse paver?
  [set pcolor yellow]
  [set pcolor scale-color white regolith 0.5 10]
  
end

to find-clusters
;;paver cluster finding function
;;reset everything
ask patches [
  set cluster nobody
  set plabel ""
  ]

loop[
  ;; pick a random patch that isn't in a cluster yet
  let seed one-of patches with[(cluster = nobody) and (paver? = true) ]
  ;; if we can't find one, then we're done!
    if seed = nobody
    [ ;;show-clusters
      stop ]
    ;; otherwise, make the patch the "leader" of a new cluster
    ;; by assigning itself to its own cluster, then call
    ;; grow-cluster to find the rest of the cluster
    ask seed
    [ set cluster self
      grow-cluster ]
  ]
end
to show-clusters
  let counter 0
  loop
  [ ;; pick a random patch we haven't labeled yet
    let p one-of patches with [plabel = ""]
    if p = nobody
      [ stop ]
    ;; give all patches in the chosen patch's cluster
    ;; the same label
    ask p
    [ ask patches with [cluster = [cluster] of myself]
      [ set plabel counter ] ]
    set counter counter + 1 ]
end

to-report randbool
  
  ifelse (random 2) = 1
  [report true]
  [report false]
end
@#$#@#$#@
GRAPHICS-WINDOW
201
10
706
536
16
16
15.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
57
18
130
51
NIL
setup\n
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
63
73
126
106
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

BUTTON
-23
177
207
211
NIL
tellproducerstomakepaver
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
-23
225
219
259
NIL
tellproducerstomakesolarcell
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

bulldozer top
true
4
Rectangle -7500403 true false 195 60 255 255
Rectangle -16777216 false false 195 60 255 255
Rectangle -7500403 true false 45 60 105 255
Rectangle -16777216 false false 45 60 105 255
Line -16777216 false 45 75 255 75
Line -16777216 false 45 105 255 105
Line -16777216 false 45 60 255 60
Line -16777216 false 45 240 255 240
Line -16777216 false 45 225 255 225
Line -16777216 false 45 195 255 195
Line -16777216 false 45 150 255 150
Polygon -1184463 true true 90 60 75 90 75 240 120 255 180 255 225 240 225 90 210 60
Polygon -16777216 false false 225 90 210 60 211 246 225 240
Polygon -16777216 false false 75 90 90 60 89 246 75 240
Polygon -16777216 false false 89 247 116 254 183 255 211 246 211 211 90 210
Rectangle -16777216 false false 90 60 210 90
Rectangle -1184463 true true 180 30 195 90
Rectangle -16777216 false false 105 30 120 90
Rectangle -1184463 true true 105 45 120 90
Rectangle -16777216 false false 180 45 195 90
Polygon -16777216 true false 195 105 180 120 120 120 105 105
Polygon -16777216 true false 105 199 120 188 180 188 195 199
Polygon -16777216 true false 195 120 180 135 180 180 195 195
Polygon -16777216 true false 105 120 120 135 120 180 105 195
Line -1184463 true 105 165 195 165
Circle -16777216 true false 113 226 14
Polygon -1184463 true true 105 15 60 30 60 45 240 45 240 30 195 15
Polygon -16777216 false false 105 15 60 30 60 45 240 45 240 30 195 15

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

container
true
0
Rectangle -7500403 false true 0 75 300 225
Rectangle -7500403 true true 0 75 300 225
Line -16777216 false 0 210 300 210
Line -16777216 false 0 90 300 90
Line -16777216 false 150 90 150 210
Line -16777216 false 120 90 120 210
Line -16777216 false 90 90 90 210
Line -16777216 false 240 90 240 210
Line -16777216 false 270 90 270 210
Line -16777216 false 30 90 30 210
Line -16777216 false 60 90 60 210
Line -16777216 false 210 90 210 210
Line -16777216 false 180 90 180 210

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

die 6
false
0
Rectangle -7500403 true true 45 45 255 255
Circle -16777216 true false 84 69 42
Circle -16777216 true false 84 129 42
Circle -16777216 true false 84 189 42
Circle -16777216 true false 174 69 42
Circle -16777216 true false 174 129 42
Circle -16777216 true false 174 189 42

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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

tile brick
false
0
Rectangle -1 true false 0 0 300 300
Rectangle -7500403 true true 15 225 150 285
Rectangle -7500403 true true 165 225 300 285
Rectangle -7500403 true true 75 150 210 210
Rectangle -7500403 true true 0 150 60 210
Rectangle -7500403 true true 225 150 300 210
Rectangle -7500403 true true 165 75 300 135
Rectangle -7500403 true true 15 75 150 135
Rectangle -7500403 true true 0 0 60 60
Rectangle -7500403 true true 225 0 300 60
Rectangle -7500403 true true 75 0 210 60

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.1.0
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
0
@#$#@#$#@
