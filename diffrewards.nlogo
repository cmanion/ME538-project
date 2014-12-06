extensions [table]

globals[
  solarcellpower
  producerpowerneededperstep
  workerbatterycapacity
  workerchargerate
  workerchargetomove ;;should eventually be proportional to how much it is carrying
  workerchargetomine 
  workerregolithcapacity ;; amount of regolith a worker can hold
  productcostinformation ;;information on how much a product cost to make for a producer
  producerregolithcapacity ;;amount of regolith a producer can store
  producerproductcapacity ;;amount of finished products a producer can store
  workeramounttomine ;; amount worker can mine each turn
  producerradioaintensity;; intensity for radio beacon a ; location beacon
  producerradiobintensity;; intensity range for regolith needed beacon
  producerradiocintensity;; intensity range for product done beacon  
  paversavailable
  regolithavailable ;; cr
  w5
  globalidletime
  utilityworkers
  utilitysolarcells
  utilityproducers
  utilitypavers
  productivity
  dpavers 
  dsolarcells 
  dworkers 
  dproducers
  dregolith
  npavers 
  nsolarcells 
  nworkers 
  nproducers
  ;prevpavers
  ;prevsolarcells
  ;prevproducers
  ;prevregolith
  qinit
  workerminingreward
  workerdeliveryreward
  regolithheld ; total regolith held
  winventory
  productionlist; how much is being produces
  inventorylist;
  ;pturns
  ;scturns
  ;wturns
  ;proturn
  ]


;NEEDS TO BE IMPLEMENTED
;for simulator:
;what the worker can sense
;worker can sense the states of the cell it is on,  the regolith values on the cells surrounding it, whether the surrounding cells are pavers, whether there are producers, solar cells on surrounding cells
;ignore other workers for now
;Q-learning: Q-learning


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
  idstack ;the IDs of the products being held
  intensitya
  intensityb
  intensityc
  idlecount
  workerQ
  Qp
  ]
workers-own[
  wregolith
  wcharge
  itemheld
  carryingpaver?
  state
  idle
  Qw
  ;prevstate ;;previous state
  actionperformed; what action the agent just successfully accomplished(IE regolith transfer)
  result; used to keep track of how much changed when the agent performed the action. IE how much regolith was mined or transfered
  wreward
  aw ; action that 
  s
  ap
  ]
to setup
clear-all
setglobals
setup-patches
set-default-shape solarcells "die 6"

;;
ifelse graphicstoggle 
[set-default-shape workers "default"
  set-default-shape producers "square"
  ]
[set-default-shape workers "bulldozer top"
  set-default-shape producers "factory"]
set-default-shape launchers "container"
setup-seed -2 0
setup-seed 1 0
recolor-all
reset-ticks
set npavers count patches with [paver?]
set nworkers count workers
set nproducers count producers
set nsolarcells count solarcells
end

to go
 addpower
 calculatepaversavailable
 calculateregolithavailable
 
 ask producers with [not hidden?]
 [
   ;;startproducingproduct 2 ;; for fun
   startproducingproduct mostvaluableproduct
   produceproduct
   producercalculatebeaconintensity
   
 ]
 
 ask patches [generate-radiofields]
 ;ask workers  with [(not hidden?) and (not ( (not paver?) and wcharge = 0)) ]
 ;[worker-learning]
 if rewardtype = 1
 [ask workers  with [(not hidden?) and (not ( (not paver?) and wcharge = 0)) ] [workercanned]]
 if rewardtype = 2
 [ask workers  with [(not hidden?) and (not ( (not paver?) and wcharge = 0)) ] [worker-learning]]
 if rewardtype = 3
 
 [worker-difference-rewards]
  if rewardtype = 4

 [worker-global-rewards]
 ;[workercanned]
 incrementproduceridlecount
 calculateglobalidletime
 if rewardtype != 3
 [calculateproductivity]
 recolor-all
 ask patches [displaypoweravailable]
 tick
end

to setglobals
  set solarcellpower 0.75
  set producerregolithcapacity 20
  set producerproductcapacity 4 ;; producers can store 4 solar cells or pavers, or 1 of anything else
  set workerbatterycapacity 4
  set workerregolithcapacity 4
  set workerchargetomine 0.05
  set workeramounttomine 1
  set workerchargerate 0.25
  set workerchargetomove 0.05
  set producerradioaintensity 10
  set producerradiobintensity 10
  set producerradiocintensity 10
  set paversavailable 0
  set regolithavailable 0
  set w5 1
  set productivity 0
  set workerminingreward 2
  set workerdeliveryreward 5
  set npavers 0
  set alpha 0.1
  set discountrate 0.4
  set qinit 1
  ;;add more global variables here
  
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
  ask workers with [wcharge = 0 and (not paver?)]
  [set color violet]
  ask workers with[wcharge > 0]
  [
    set color red
    ]
  ask patches
  [
    ifelse beaconmode? [recolor-patcheswithbeacon]
    [recolor-patches]]
end





to qtest
;; function for testing Q
  setglobals
  let Q table:make
  set s (list 1 2 3 4 5)
  set ap (list 1 2 3)
  let foo maximumQvalueandaction Q s ap
  print foo
  print Q
  updateQ Q s 1 5 ap
  print Q
end

to worker-difference-rewards
  ;; run workers
  ask workers with [(not hidden?) and (not ( (not paver?) and wcharge = 0)) ]
  [
  set ap workerpossibleactions
  let prevradio-a radio-a
  set s sensorscan
  set aw item 0 maximumQvalueandaction Qw s ap
  if not randpercent
  [
    set aw one-of ap  
  ]
  workerperformaction aw ;; greedily choose the action with the highest value
  if (radio-a > prevradio-a)
  [
  set wreward wreward + ((workerbatterycapacity - wcharge) / workerbatterycapacity) ; don't die potential  
  ]
  if (radio-a > prevradio-a) and wregolith > 0 [set wreward wreward + (wregolith / workerregolithcapacity)] ; go home potential
  
  ]
  ;let fakepro 0
  
  calculateproductivity
  ask workers with [(not hidden?) and (not ( (not paver?) and wcharge = 0)) and (s != 0) ]
  [
    
  set wreward wreward + 10 * (productivity - productivitywithoutworker)
  
  updateQ Qw s aw wreward ap
  set wreward 0
  set actionperformed 0
  set result 0
  ]
end


to worker-global-rewards
  ;; run workers
  ask workers with [(not hidden?) and (not ( (not paver?) and wcharge = 0)) ]
  [
  set ap workerpossibleactions
  let prevradio-a radio-a
  set s sensorscan
  set aw item 0 maximumQvalueandaction Qw s ap
  if not randpercent
  [
    set aw one-of ap  
  ]
  workerperformaction aw ;; greedily choose the action with the highest value
   let drad discountrate * sign (radio-a - prevradio-a)

  set wreward wreward + drad * ((workerbatterycapacity - wcharge) / workerbatterycapacity) ; don't die potential  
 
  set wreward wreward + drad * (wregolith / workerregolithcapacity) ; go home potential
  
;  if (radio-a > prevradio-a)
;  [
;  set wreward wreward + ((workerbatterycapacity - wcharge) / workerbatterycapacity) ; don't die potential  
;  ]
;  if (radio-a > prevradio-a) and wregolith > 0 [set wreward wreward + (wregolith / workerregolithcapacity)] ; go home potential
;  
;  ]
  ;let fakepro 0
  ]
  calculateproductivity
  ask workers with [(not hidden?) and (not ( (not paver?) and wcharge = 0)) and (s != 0) ]
  [
    
  set wreward wreward + 10 * (productivity)
  
  updateQ Qw s aw wreward ap
  set wreward 0
  set actionperformed 0
  set result 0
  ]
end
to-report sign [x]
  if x = 0 [report 0]
 ifelse x >= 0
 [report 1]
 [report -1]
end
to worker-learning
  ;figure out what the worker can dow
  
  set ap workerpossibleactions
  let prevradio-a radio-a
  ;get the state of the worker
  ;let s (list random 3 random 3 random 3 random 3 random 3)
  set s sensorscan
  ;print s
  set aw item 0 maximumQvalueandaction Qw s ap
  if not randpercent
  [
    set aw one-of ap  
  ]

  ;print a
  workerperformaction aw ;; greedily choose the action with the highest value
  ;updateQ with the reward(
  
  if actionperformed = 1
  [ set wreward result * workerminingreward] ; reward for mining
  if actionperformed = 2 
  [set wreward result * workerdeliveryreward]; reward for delivering stuff
  if member? actionperformed [3 4 5] ;reward for picking stuff up
  [set wreward result * 1.1]
  if member? actionperformed [6 7 8]; reward for placing stuff
  [set wreward result * 2]
 
   
  if (radio-a > prevradio-a)
  [
  set wreward wreward + ((workerbatterycapacity - wcharge) / workerbatterycapacity)  
  ]
  if (radio-a > prevradio-a) and wregolith > 0 [set wreward wreward + (wregolith / workerregolithcapacity)]
  ;workerpowerdepletionpenalty does not seem to work well
  updateQ Qw s aw wreward ap
  ;let
  ;show Qw
  ;show table:from-list (table:to-list Qw) 
  set wreward 0
  set actionperformed 0
  set result 0
  
end

to updateQ [Q sl al r apl]
  ;; Q is qtable
  ;;s states
  ;;a action
  ;;r reward
  ;;updates a Q matrix
  ;;r is reward received
  let a-q table:get Q sl ;; list of actions and Q values
  let index position aw map first a-q ;; the the index of the action and q-value
  let curQ item 1 item index a-q;;current Q value
  let maxQ item 1 maximumQvalueandaction Q sl apl
  let nexQ (curQ + alpha *( r + discountrate * maxQ - curQ))
  set a-q (replace-item index a-q (replace-item 1 (item index a-q) nexQ))
  table:put Q sl a-q
  
end

To-report maximumQvalueandaction [Q sl apl]
  ;;finds the maximum Q value and action given a Q matrix, current state, possible actions
  ;;Q is a table with keys as states and a list of actions and Q values
  ;;ap are the possible actions, 
  ;;ap list of integers
 ; if not table:has-key? Q
 ;qa is the list of actions and Q values
   if not table:has-key? Q sl
  [
    table:put Q sl (list)
  ]
  let a-q table:get Q sl
  ;;initialize Q
  set a-q initializeQactions ap a-q
  table:put Q sl a-q
  ;;find a list of action Q values only from the actions that are available
  let filteractions filter [member? first ? apl] a-q
  let index indexofmaxvalueinlist map last filteractions;; 
  report item index filteractions  
end

to-report maxQandactiongivenpossibleactions [apl a-q]
  ;;a-q must already be initialized
  ;;maxQandactiongivenpossibleactions (list 1) (list (list 1 1) (list 2 3))
  ;;
  ;;ap are the possible actions, qa is the list of actions and Q values
  ;;ap list of integers
  ;;a-q list( (list action qvalue) )

  ;let plist 
  ;;find a list of action Q values only from the actions that are available
  let filteractions filter [member? first ? apl] a-q
  let index indexofmaxvalueinlist map last filteractions;; 
  report item index filteractions
end

to-report initializeQactions[apl a-q]
  let defaultq qinit
  ;; initialize an action if it isn't in the list
  foreach apl
  [
    ;; find if a value is not in the first part of the lsit
  if not member? ?1 map first a-q
  [
   ifelse (?1 = 8) or (?1 = 9) ; worker potential to pick stuff up
   [set a-q lput (list ?1 (10 * defaultq)) a-q]
   [set a-q lput (list ?1 defaultq) a-q]
   ;initialize an action that isn't in the list to initial q value   
  ]
    
  ]
  report a-q
end
to workerpowerdepletionpenalty
  if wcharge <= 0
  [ 
  move-to one-of patches with[paver?]
  set wregolith 0
  set wreward 0 
  ]
end
to-report indexofmaxvalueinlist [inlist]
;let m max inlist
;find the index of one of the maximum values in a list
;to calculate this I have to do some weird stuff
;gotta make a new list of lists first entry as inlist values and second value is list index
;then do with-max-first! it's that easy(or complicated)
let fakelist []
let index 0
foreach inlist
[
  let foolist (list ? index)
  ;print foolist
  set fakelist lput (list ? index) fakelist
  ;print ?
  set index index + 1
]

report with-max-first fakelist

;report position m inlist
end
to calculatepaversavailable
  set paversavailable 0
  ask patches with [paver?]
  [
   if (count producers-here with [not hidden?] = 0) and (count solarcells-here with [not hidden?] = 0)
   [
     set paversavailable paversavailable + 1
   ] 
  ]

end

to-report whatsonthepatch [p]
  let sccount count (solarcells-on p) with [not hidden?]
  let procount count (producers-on p) with [not hidden?]
  ifelse ((sccount > 0) or (procount > 0 ))
  [
    if (sccount > 0)
    [report 3]
    if (procount > 0)
    [report 4]
  ]
  [
    let fakepaver? false ; check if the patch ahead has a paver
    ask p [set fakepaver? paver?]
    
    ifelse fakepaver?
    [
      report 2  
    ]
    [
      ifelse regolith > 0
      [report  (ceiling (4 * (regolith / 10) )) / 4 ];discretization into 4 different values
      [report 0]
    ]
    ;then paver
  ]
  
  ;p is a patch
  
  
  report [];; there's nothing on the patch
end

to-report sensorscan
  let sensorlist []
  set sensorlist lput (whatsonthepatch patch-here) sensorlist 
  foreach [0 1 2 3 4 5 6 7]
  [
    set heading ? * 45
    set sensorlist lput (whatsonthepatch patch-ahead 1 ) sensorlist 
  ]
  set sensorlist lput ((ceiling (4 * (wcharge / workerbatterycapacity) )) / 4) sensorlist
  ifelse itemheld != nobody
  [set sensorlist lput 1 sensorlist]
  [set sensorlist lput 0 sensorlist]
  ifelse count workers-here > 1
  [set sensorlist lput 1 sensorlist]
  [set sensorlist lput 0 sensorlist]
  report sensorlist
  
  
end


to workerperformaction [action]
  ;;perform a discrete action
  workerrecharge
  if (action >= 0) and (action < 7)
  [
   moveworker action
    
   ]
  if action = 7
  [
    mine
  ]
  if action = 8
  [
    transferregolithtoproducer  
  ]
  if action = 9
  [
    pickupproductfromproducer
  ]
  if action = 10
  [
    putdown  
  ]
  
  if action = 11
  [
    downhillradioa  
  ]
  
  if action = 12 
  [
    uphillradioa  
  ]
  
  if action = 13
  [
    gohomemining
  ]
  if action = 14
  [
    gohomeplace  
  ]
end

to-report workerpossibleactions
  ;; we limit the actions a worker can perform to make learning more efficient
  let alist (list 0 1 2 3 4 5 6 7)
  if not paver?
  [
  set alist lput 7 alist ; if the worker is not on a paver it can mine
    
  ]
  if workerhome; if the worker is at a producer
  [
     if wregolith > 0
     [set alist lput 8 alist]
     if one-of producers-here with[ (not hidden?) and (length productstack > 0)] != nobody
     [
     set alist lput 9 alist
     ]
  ]
  if carryingpaver? and canplacepaver
  [
    set alist lput 10 alist
  ]
  if itemheld != nobody and (not carryingpaver?)
  [
    set alist lput 10 alist  
  ]  
  
  set alist lput 11 alist
  set alist lput 12 alist
  set alist lput 13 alist
  set alist lput 14 alist
  report alist
end

to-report producerpossibleactions
let alist []

if (capacity > 0) and productid = -1
[set alist lput 0 alist
 set alist lput 1 alist
 
 if capacity >= 4
 [
   
   set alist lput 2 alist
   set alist lput 3 alist
 ] 
 
 ]
 if productid > -1
 [
 set alist lput 6 alist ;opt not to produce product
 ]
 set alist lput 7 alist ; null action, just produce the product and nothing else 
 set alist lput 8 alist;
 set alist lput 9 alist; tell a small lie
 set alist lput 10 alist;
 set alist lput 11 alist; tell a big lie 
report alist
end

to performactionproducer [action]
   
   if action != 6 [produceproduct]
   if action <= 3
   [
    startproducingproduct action  
   ]
   
   if action = 8  
   [set intensityb intensityb + 2] ;;lie about how badly regolith is needed by a small amount
   if action = 9
   [set intensityc intensityc + 2] ; lie about how badly inventory needs to be removed by a small amount
   if action = 10  
   [set intensityb intensityb + 5] ;;lie about how badly regolith is needed by a large amount 
   if action = 11
   [set intensityc intensityc + 5] ; lie about how badly inventory needs to be removed by a large amount
   
end


to calculateglobalidletime
  set globalidletime 0
  let pcount count producers with [not hidden?]
  ask producers with [not hidden?]
  [
  set globalidletime globalidletime + idlecount  
    
  ]
  set globalidletime globalidletime / pcount
end

to calculateregolithavailable
  set regolithavailable 0
  ask producers with [not hidden?]
  [
  set regolithavailable regolithavailable + producerregolithcapacity - pregolith  
    
  ]
end



to generate-radiofields ;;patch procedure for setting up radiofields
  set radio-a 0
  ask producers with[ not hidden?]
  [set-radiofielda myself] ;; just setting up radiofield a for now
  
end

to set-radiofielda[p] ;;turtle procedure, p is patch copied from moths example
  let rsquared (distance p) ^ 2
  let amount intensitya
  ifelse rsquared = 0
    [ set amount amount * 1000 ]
    [ set amount amount / rsquared ]
  ask p [ set radio-a radio-a + amount ]
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
  create-workers 2
  [
   setxy x y
   set heading 0
   set wcharge 1
   initializeworker
  
  ]
    
end

to incrementproduceridlecount
  ask producers with [not hidden?]
  [
  if capacity = 0
    [
      if idlecount <= 10[
      set idlecount idlecount + 1]
    ]
    
  ]
end

;;producer functions
;;the only action available to the producer is to start making a product with the start making product function

;to producercannedmode
     
;end

to producercalculatebeaconintensity ;;producer procedure
  set intensityb round (producerradiobintensity * ((producerregolithcapacity - pregolith) / producerregolithcapacity))
  set intensityc round (producerradiocintensity * ((producerproductcapacity - capacity) / producerproductcapacity))
  
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
       set idstack lput productid idstack
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
     set idstack lput productid idstack
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
     set idstack lput productid idstack
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
     set idstack lput productid idstack
     set productid -1
     set capacity capacity - capacitycost  
     ]
     if productid = 2 ;making a worker, workers automatically drive off
     [
       let Qtoset workerQ
       hatch-workers 1
       [
         initializeworker
         set Qw table:from-list (table:to-list Qtoset) ;this way we aren't using a table immutably
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
   ;;start producing a product, indicated by the product id
   ;;might make this a bool
set idlecount 0  
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

;;worker functions

to pickupproductfromproducer ;; worker function
  if (not carryingpaver?) and (itemheld = nobody); ifthe worker can carry stuff
  [
    let Qtoset Qw;qmatrix from worker
    let producerhere one-of producers-here with[(not hidden?) and (capacity < producerproductcapacity)] ;switch to producer 
    if producerhere != nobody[
    ask producerhere
    [
      let costvector item (last idstack) productcostinformation
      let capacitycost item 3 costvector
      let product last productstack  ;get the product on the top of the stack
      ;;let lastitem (length productstack - 1)
      set productstack remove-item (lastitem productstack) productstack;; remove the item from the product stack
     ; show productstack
      
      set capacity (capacity + capacitycost)
      set idstack remove-item (lastitem idstack) idstack; remove the id 
      set workerQ Qtoset
      ask myself; switch back to worker
      [
          set result 1
          ifelse product = 1
          [
            set carryingpaver? true
            set actionperformed 3
          ]
          [
            if is-solarcell? product [set actionperformed 4]
            if is-producer? product [set actionperformed 5]
          ]
          set itemheld product  
         
      ]
    ]
    ]
  ]
end

to putdown

    ;function for the workers to put down items
    ifelse (not paver?)[
      if (itemheld = 1)
      [
        set carryingpaver? false
        set paver? true
        set pcolor yellow
        set itemheld nobody
        set actionperformed 6
        set result 1
      ]
    ]
    [
    let scount count solarcells-here with [not hidden?]
    let pcount count producers-here with [not hidden?]
    let lcount count launchers-here with [not hidden?]
    if (scount = 0) and (pcount = 0) and (lcount = 0)
    [
        if is-turtle? itemheld
        [
          ask itemheld [move-to myself]
          ask itemheld [st] ;show the item
          set result 1
         
          if is-solarcell? itemheld [set actionperformed 7]
          if is-producer? itemheld [set actionperformed 8 ]
          set itemheld nobody
          
        ]
    ] 
    ]

  
end 
to workercanned
  ifelse wcharge < (workerbatterycapacity / 2)
  [uphillradioa]
  [if state = 1
  [miningmodecanned]
  if state = 2
  [pickandplacecanned]]
  workerrecharge
end
to pickandplacecanned
  ifelse itemheld = nobody
  [ifelse workerhome
    [pickupproductfromproducer
    if itemheld = nobody
    [set state 1 ] 
    ]
    [gohomeplace]
    ]
  [ifelse itemheld != 1
    [
    findaplaceforfactoryitem  
    ]
    [findaplaceforpaver]
  ]
end

to findaplaceforpaver
  ifelse paver?
  [ifelse onedge
    [downhillregolithnopavers]
    [moverandomlyonpavers]
    ]
  [ifelse canplacepaver
    [putdown
      set state 1
      ]
    [uphill radio-a]
    ]
  
end

to findaplaceforfactoryitem
  ifelse paver?
  [ifelse paveroccupied
    [moverandomlyonpavers]
    [putdown
      set state 1]
    ]
  [
    uphill radio-a
    ]
end
to moverandomlyonpavers
  let pavers neighbors with [paver?]
  move-to one-of pavers 
end




to miningmodecanned
  ifelse wregolith < workerregolithcapacity
  [
    ifelse ( paver? )
    [getoffpavers]
    [
      ifelse((regolith <= 0))
      [moveworker random 7]
      [movemine]
    ]
  ]
  [
    ifelse workerhome
    [transferregolithtoproducer
      set state 2]
    [gohomemining]
    
  ]
end

to gohomemining
  movetowardagent2 producerwithmaxradiob
end

to gohomeplace
  movetowardagent2 producerwithmaxradioc
end 

to movetowardagent [a] 
let p min-one-of neighbors[distance a]
if [distance a] of p < (distance a) 
[
  face p
  move-to p
]
end
to movetowardagent2 [a]
let p min-one-of neighbors[distance a]
if [distance a] of p < (distance a) 
[
  
  move-tousingpower p
]
end
to-report workerhome
  ifelse ( count producers-here with [ not hidden?]) > 0
  [report true]
  [report false]
end

to-report paveroccupied
  ifelse ((( count producers-here with [ not hidden?]) > 0) or (( count solarcells-here with [ not hidden?]) > 0))
  [report true]
  [report false]
end

to getoffpavers
  ;;worker function to get off the paverse
  ifelse random 2 > 0
  [downhillradioa]
  [moverandomlyonpavers]
end
to-report canplacepaver
  let paves neighbors4 with[paver?]
  ifelse ((count paves > 0) and (not paver?))
  [report true]
  [report false]
end

to-report onedge ;; if worker is on an edge
  
  let paves neighbors4 with[not paver?]
  ifelse count paves > 0 and paver?
  [report true]
  [report false]
end

to movemine
  uphillregolithnopavers
  mine
end

to uphillregolithnopavers
  let p max-one-of neighbors with [not paver? ] [(regolith)]
if [regolith] of p > (regolith) 
[
  face p
  move-to p
]
end

to uphillradioa
  move-tousingpower patch-here
  let p max-one-of neighbors [radio-a]
  if [radio-a] of p > radio-a [
    move-tousingpower p
    
    ]
end


to downhillradioa
  move-tousingpower patch-here
  
  let p min-one-of neighbors [radio-a]
  ifelse [radio-a] of p < radio-a [
    move-tousingpower p
    
    ]
  [move-tousingpower one-of neighbors]
end

to downhillregolithnopavers
  let p min-one-of neighbors4 with [not paver? ] [(regolith)]
if [regolith] of p < (regolith) or paver? 
[
  face p
  move-to p
]
end

to mine
  ;; what needs to be done here
  ;;modify so that worker cannot mine more regolith than it can hold
  ;;modify so that worker cannot set regolith to negative
  
  if ((regolith > 0) and (not paver?) and (workerusecharge workerchargetomine) and (wregolith < workerregolithcapacity))
  [
    ;;set remaining regolith - workeramounttomine ;
    let amountmineable workeramounttomine; how much can we mine
    if (workeramounttomine + wregolith) > workerregolithcapacity
    [
     set amountmineable workerregolithcapacity - wregolith
    ]
    if (regolith - amountmineable) < 0
    [
     set amountmineable regolith  
    ]
    set regolith regolith - amountmineable ;; worker can mine it to be less than 0, need to change this, but it should be fine for now
    set wregolith wregolith + amountmineable ;; BUG HERE REMOVE BUG
    ;set wreward amountmineable * workerminingreward
    set actionperformed 1
    set result amountmineable
  ]
end

to workerrecharge
  if (( paver?) and (wcharge < workerbatterycapacity)) ;; if the worker is on a paver attempt to recharge
  [
      if (usepower workerchargerate)
      [
       set wcharge wcharge + workerchargerate  
      ]
  ]
end
to transferregolithtoproducer
  let producerhere one-of producers-here with [(not hidden?) and (pregolith < producerregolithcapacity)] ;; get a producer here that is active 
  let wreg wregolith ; possibly redundant
  let regtransfer 0
  if producerhere != nobody
  [
    ask producerhere
    [
       let regcapacityavail producerregolithcapacity - pregolith ;; amount of capacity available
       ifelse regcapacityavail < wreg
       [
           set regtransfer regcapacityavail
       ]
       [
           set regtransfer wreg       ;;determine amount of regolith that can be transfered to the producer
       ]
       set pregolith pregolith + regtransfer ;; give the producer regolith
       ask myself
       [
          set wregolith wregolith - regtransfer
          ;set wreward regtransfer * workerdeliveryreward
          set actionperformed 2
          set result regtransfer  
       ]
    ]
  ]
  ;if patch contains producer
  ;get producer
  ;ask producer how much regolith it has
  
end
;;worker beacon sensors

to-report maxradiob
  let maxproducer producerwithmaxradiob ;; find the max radio beacon value
  let intensity 0
  if maxproducer != nobody
  [
    ask maxproducer
    [
    set intensity intensityb
      
    ]
    report intensity
 ]
end

to-report producerwithmaxradiob
  report max-one-of producers with [not hidden? ] [intensityb]
  
end

to-report maxradioc
  let intensity 0
  let maxproducer producerwithmaxradioc ;; find the max radio beacon value
  if maxproducer != nobody
  [
    ask maxproducer
    [set intensity intensityc]
    report intensity
    
 ]
end

to-report producerwithmaxradioc
  report max-one-of producers with [not hidden? ] [intensityc]
  
end

to-report workerusecharge [amount]
  ;uses amount of charge from the workers battery and returns true if this amout can be provided
  ifelse amount > wcharge
  [
  report false
  ]
  [
    set wcharge wcharge - amount 
    report true
  ]
end

to moveworker [dir]
  ;;move the worker N, E, S, W or a in a diagonal, specified with an integer
  set heading dir * 45
  ifelse (dir mod 2) = 1
  [movedistanceusepower sqrt 2]
  [movedistanceusepower 1]
  ;[fd sqrt 2]
  ;[fd 1]
  if is-turtle? itemheld
  [ask itemheld[move-to myself]]; move item with turtle
end

to movedistanceusepower [dist]
  ;;move the worker and use power while moving if power is available
  let powertomove dist * workerchargetomove
  
  ifelse powertomove > wcharge
  [
   ; move until all the charge is used, don't move
   set wcharge 0
   ;fd wcharge / workerchargetomove 
   
  ]
  [
    fd dist
    set wcharge wcharge - powertomove
  ]
end

to move-tousingpower [p] ;move-to equivalent for the worker that uses charge
  
  let dist distance p
  face p
  movedistanceusepower dist
end


;;function for anything on a paver to use power off the grid
to-report usepower [amount]
  if paver?
  [
    let currentcluster cluster
    ifelse poweravailable > amount
    [
      
      ask patches with [cluster = currentcluster]
      [
        set poweravailable poweravailable - amount
        
      ]
      report true
    ]
    [  
          
      ask patches with [cluster = currentcluster]
      [
        set poweravailable 0
        
      ]
      report false
      ]
  ]
  report false
end

to-report mostvaluableproduct
   
    let utilitypaver  1.1 / (paversavailable + 0.01)
  ; capacity remaining
    let utilityworker globalidletime * w5
    let utilityproducer poweravailable / ( regolithavailable + 0.01)
    let utilitysolarcell 1 / (poweravailable + 0.01)
   let utilitylist (list
     list utilitypaver 0
     list utilitysolarcell 1
     list utilityworker 2
     list utilityproducer 3   
     )  
   report with-max-first utilitylist
   
    
    
end

to-report with-max-first [lists]
  let highest-first max map first lists
  let winning? task [first ? = highest-first]
  report first butfirst one-of filter winning? lists
end 
;to-report producerreward [ preward ]
 ; let pa poweravailable
  ;let globalideltime  5 ; needs to be changed
  ;let pva 2  ;needs to be changed 
  ;let w5 0.4 ; needs to be changed
  ;let cr 4 ; needs to be changed 
  
  ;let usc 1 / (pa + 0.1)
  ;let 
  ;let upav 1 / (pva + 0.1)
  ;let upro pa / (cr + 0.1)
  ;set preward usc + uw + upav + upro
  
;end

;to-report workersrewards [wreward]
 ; let ecost w2 / poweravailable
 ; set wreward (regdelivered * w1) + rplacepaver + rplaceelement -(eused * ecost) - sfailure
  
;end

;;initialization functions
to initializeproducer
    set heading 0
    set color green
    set productid -1
    set productstack []
    set idstack []
    set capacity producerproductcapacity
    set intensitya producerradioaintensity
    set pregolith 0; 
    set idlecount 0
end

to initializeworker
  set itemheld nobody
  set color red
  set carryingpaver? false
  set state 1
  set wreward 0
  set Qw table:make
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
    set plabel round poweravailable
  ]
  [
    set plabel ""
  ]
end

to calculateproductivity
  set productionlist produceroperation ;;list of stuff that is currently being made
  set inventorylist producerinventory ;; list of stuff held in inventory
  set winventory workerinventory ;; list of stuff carried by workers
  let pbeingmade item 0 productionlist
  let scbeingmade item 1 productionlist
  let wbeingmade item 2 productionlist
  let probeingmade item 3 productionlist
  let carriedpavers 0
  let producerpavers 0
  let placedpavers count patches with [ paver? ]
  let placedsolarcells count solarcells with [not hidden?]
  let placedproducers count producers with [not hidden?]
 
  set carriedpavers item 0 winventory
  
  let curregolith 0.5 * (item 4 winventory) + (item 4 producerinventory)  
  set dregolith abs( curregolith - regolithheld)
  if dregolith < 0 [set dregolith 0]
  
  set regolithheld curregolith 
  let currentp  placedpavers + 0.75 * carriedpavers + 0.5 *(item 0 inventorylist) + 0.25 * pbeingmade 
  set dpavers currentp - npavers 
  set npavers currentp ;update number of pavers
  
  let sccount placedsolarcells + 0.75 *(item 1 winventory) + 0.5 * (item 1 producerinventory) + 0.25 * scbeingmade 
;;  ask patches [
;;    let solarcellsplaced count patches with  [ solarcells? ]
;;  ]
;  ask producer [
;    let solarcellsproducing count producer with [ solarcells ]
;  ]

;  let x2 foo2 + solarcellsplaced + solarcellsproducing
  set dsolarcells sccount - nsolarcells
  set nsolarcells sccount
  let wcount (count workers) + wbeingmade
  set dworkers wcount - nworkers
  set nworkers wcount
;  
;  ask producer [
;    let workerproducing count producer with [ productid 2 ]
;  ]
;  let x3 workerworking + workerproducing
;  set dworkers x3 - nworkers
;  
  let procount placedproducers + 0.75 * (item 3 winventory) + 0.5 * (item 3 producerinventory) + 0.25 * probeingmade
;  ask patches [
;    let producerspresent count patches with [ producer? ]
;  ]
;  ask producers [
;    let newproducers count producer with [ productid 3 ]
;  ]

;  let x4 foo4 + producerspresent + newproducers
  set dproducers procount - nproducers
  set nproducers procount
;  
;  
;  
set productivity (dregolith + dpavers + dsolarcells + dworkers + dproducers) / ( (placedpavers + placedsolarcells + (count workers) + placedproducers))
end

to-report productivitywithoutworker ;worker function
  ; this function assumes calculate productivity has already been run
  ;let productionlist produceroperation ;;list of stuff that is currently being made
  ;let inventorylist producerinventory ;; list of stuff held in inventory
  ;set winventory workerinventory ;; list of stuff carried by workers
  let pbeingmade item 0 productionlist
  let scbeingmade item 1 productionlist
  let wbeingmade item 2 productionlist
  let probeingmade item 3 productionlist
  let carriedpavers 0
  let producerpavers 0
  let placedpavers count patches with [ paver? ]
  let placedsolarcells count solarcells with [not hidden?]
  let placedproducers count producers with [not hidden?]
  set carriedpavers item 0 winventory
  let workerregolith (item 4 winventory)
  let producerregolith (item 4 producerinventory)
  if actionperformed = 1
  [set workerregolith workerregolith - result]
  if actionperformed = 2
  [set producerregolith producerregolith - result]
  let curregolith 0.5 * workerregolith + producerregolith
  let paversinstorage (item 0 inventorylist)
  let dregolithl abs(curregolith - regolithheld)
  if dregolith < 0 [set dregolith 0]
  
  ;;set regolithheld curregolith 
  if actionperformed = 3
  [ 
    set carriedpavers carriedpavers - 1 
    ;set paversinstorage paversinstorage + 1
  ]
  if actionperformed = 6
  [
    set placedpavers placedpavers - 1
    ;set carriedpavers carriedpavers + 1 
  ]
  let currentp  placedpavers + 0.75 * carriedpavers + 0.5 * paversinstorage + 0.25 * pbeingmade 
  let dpaversl currentp - npavers 
  
  let workersolarcells item 1 winventory
  let producersolarcells item 1 producerinventory
  if actionperformed = 4
  [
    set workersolarcells workersolarcells - 1
    ;set producersolarcells producersolarcells + 1
  ]
  if actionperformed = 7
  [set placedsolarcells placedsolarcells - 1]
  let sccount placedsolarcells + 0.75 * workersolarcells + 0.5 * producersolarcells + 0.25 * scbeingmade 
  
  let dsolarcellsl sccount - nsolarcells
 
  let wcount (count workers - 1) + wbeingmade
  let dworkersl wcount - nworkers
  
;  

  let workerproducers item 3 winventory
  let producerproducers item 3 producerinventory
  if actionperformed = 5
  [
    set workerproducers workerproducers - 1
    ;set producerproducers producerproducers + 1
  ]
  if actionperformed = 8
  [set placedproducers placedproducers - 1]
  let procount placedproducers + 0.75 * workerproducers + 0.5 * producerproducers + 0.25 * probeingmade
  

;  let x4 foo4 + producerspresent + newproducers
  let dproducersl procount - nproducers
  
;  
;  
;  
report (dregolithl + dpaversl + dsolarcellsl + dworkersl + dproducersl) / ( (placedpavers + placedsolarcells + (count workers - 1) + placedproducers))
end


to recolor-patches
  ifelse paver?
  [set pcolor yellow]
  [set pcolor scale-color white regolith 0.5 10]
  
end

to recolor-patcheswithbeacon
  ifelse paver?
  [set pcolor yellow]
  [set pcolor scale-color white radio-a 0.1 10]
  
end

to-report producerinventory
  ;take inventory of producers
  let pcount 0
  let sccount 0
  let wcount 0
  let procount 0
  let rcount 0
  ask producers with[(not hidden?) and (length idstack > 0)]
  [
    set pcount pcount + (length filter [? = 0] idstack)
    set sccount sccount + (length filter [? = 1] idstack)
    set wcount wcount + (length filter [? = 2] idstack)
    set procount procount + (length filter [? = 3] idstack)
    set rcount rcount + pregolith 
  ]
  report (list pcount sccount wcount procount rcount)
end

to-report produceroperation
;;the amount of a product being made

let pcount 0
let sccount 0
let wcount 0
let procount 0
ask producers with[not hidden? and productid > -1]
 [
      
      let costvector item productid productcostinformation
      let turns item 2 costvector ;; total turns to produce product
      
      let percomplete  (turns - turnsleft) / turns ;;percent complete
      if productid = 0
      [set pcount pcount + percomplete ]
      if productid = 1
      [set sccount sccount + percomplete]
      if productid = 2
      [set wcount wcount + percomplete ]
      if productid = 3
      [set procount procount + percomplete]
      
 ]
report (list pcount sccount wcount procount)

end
to-report workerinventory
let rcount 0
let pcount count workers with [ carryingpaver? ]
let sccount count workers with [(not hidden?) and (itemheld != nobody) and (itemheld != 1) and (member? itemheld solarcells)]
;let wcount count workers with [(itemheld != nobody) and (itemheld != 1) and (member? itemheld solarcells)]
let procount count workers with [(not hidden?) and (itemheld != nobody) and (itemheld != 1) and (member? itemheld producers)]
ask workers with [not hidden?] ;; determine how much regolith is held by workers
[
  set rcount rcount + wregolith  
]
report (list pcount sccount 0 procount rcount)

 ;ask workers with[(not hidden?) and  (itemheld != nobody)]
 ;[
  ; if itemheld  
   
 ;]

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

to-report randpercent
  
  ifelse (random-float 1) > randompercent
  [report true]
  [report false]
end

to-report randbool
  
  ifelse (random 2) = 1
  [report true]
  [report false]
end

to-report lastitem [a_list]
  ;gets the index of the last item in a list
  report (length a_list - 1)
end
@#$#@#$#@
GRAPHICS-WINDOW
212
10
1137
956
30
30
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
-30
30
-30
30
1
1
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
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1013
10
1243
44
make pavers
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
1016
49
1258
83
make solar cell
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

MONITOR
47
283
151
328
NIL
globalidletime
17
1
11

SLIDER
22
189
194
222
discountrate
discountrate
0
1.5
1.4
0.05
1
NIL
HORIZONTAL

SLIDER
23
155
195
188
alpha
alpha
0
1
1
0.05
1
NIL
HORIZONTAL

SLIDER
21
227
194
260
randompercent
randompercent
0
1
0.2
0.05
1
NIL
HORIZONTAL

PLOT
862
108
1062
258
Productivity
time
productivity
0.0
10.0
0.0
0.5
true
false
"" ""
PENS
"default" 1.0 0 -5298144 true "" "plot productivity"

MONITOR
47
336
185
381
NIL
productivity
10
1
11

SWITCH
849
10
1007
43
beaconmode?
beaconmode?
1
1
-1000

MONITOR
876
408
974
453
worker count
count workers with [not hidden?]
17
1
11

MONITOR
876
359
985
404
solarcell count
count solarcells with [not hidden?]
17
1
11

MONITOR
874
457
986
502
producer count
count producers with [not hidden?]
17
1
11

MONITOR
874
306
964
351
paver count
count patches with [paver?]
17
1
11

SWITCH
850
53
1015
86
graphicstoggle
graphicstoggle
0
1
-1000

PLOT
1022
265
1222
415
Producers
time
producers
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count producers with [not hidden?]"

CHOOSER
21
109
196
154
rewardtype
rewardtype
1 2 3 4
1

MONITOR
877
512
980
557
dead workers
count workers with [wcharge = 0]
17
1
11

BUTTON
61
391
164
424
NIL
recolor-all
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
31
449
203
482
radiothreshold
radiothreshold
0
100
35
1
1
NIL
HORIZONTAL

SWITCH
43
503
184
536
showpaver?
showpaver?
0
1
-1000

MONITOR
1155
688
1214
733
state 1
count workers with[ state = 1]
17
1
11

MONITOR
1181
599
1240
644
state 2
count workers with[ state = 2]
17
1
11

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
