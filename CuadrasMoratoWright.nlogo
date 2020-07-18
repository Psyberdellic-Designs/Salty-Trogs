breed [traders trader]

traders-own
[
  good-held            ; what's in my sack
  good-produced        ; what I make
  good-desired         ; what I want next
  utility              ; how much I've eaten
  series-payoff        ; how much I've eaten in current series
  max-series-payoff    ; how much I could've eaten in current series
  no-propensity        ; tendency to not speculate
  yes-propensity       ; tendency to speculate
  ;no-prob             ; probability of not speculating
  trade-probability    ; probability of speculating
  regret               ; regret over my speculation choice
  partner              ; my current trade partnerask partner
  previous-good        ; good offered in last trade attempt
  yes-payoff           ; payoff if I always trade
  no-payoff            ; payoff if I never trade
]

globals
[
   marginal-utility    ; utility gained from consuming one unit
   production-cost     ; utility lost from producing one unit
   num-goods           ; number of types of goods & traders
   series              ; series number
  ; series-utility      ; total utility this series
   sugar-demand        ; demand for sugar
   spice-demand        ; demand for spice
   money               ; list of # times each good used as money
   offers              ; list of # times a speculative good was offered
   salt
   sugar
   spice
]

;===========================
; initializers
;===========================

to init-model
  clear-all            ; get rid of everything from last run
  random-seed new-seed ; re-seed random number generator
  reset-ticks          ; reset clock (and plots)
  init-globals         ; initialize globals

  create-traders num-salt-producers
  [
    set good-produced salt
    set color green
    init-trader
  ]
  create-traders num-sugar-producers
  [
    set good-produced sugar
    set color red
    init-trader
  ]
  create-traders num-spice-producers
  [
    set good-produced spice
    set color blue
    init-trader
  ]

  let circle-radius 10 ; for now
  layout-circle traders circle-radius

  ask patches [init-patch]

end

to init-globals
  set num-goods 3   ; 0 = salt, 1 = sugar, 2 = spice
  set marginal-utility 1
  set production-cost 0 ; for now
  set series 0
  ;set series-utility 0
  set sugar-demand sugar-share * (1 - salt-demand)
  set spice-demand 1 - salt-demand - sugar-demand
  set money n-values num-goods [0]
  set offers n-values num-goods [0]
  set salt 0
  set sugar 1
  set spice 2
end

to init-patch
  set pcolor white
end

to init-trader
  set utility 0
  set series-payoff 0
  set max-series-payoff 0
  set good-held good-produced
  set no-propensity 0
  set yes-propensity 0
  set trade-probability .5
  ;set no-prob .5
  set regret 1
  set shape "Person"
  set partner nobody
  set yes-payoff 0
  set no-payoff 0
  set previous-good good-produced
end

;============================
; updaters
;============================

to update-model
  update-globals
  ifelse beta < random-float 1
  [
    set series series + 1
    ask traders [reset-trader]
  ]
  [
    ask traders [update-desire]       ; taste shock
    ask traders [update-partner]      ; pair-up
    ask traders [enter-market]        ; try to trade
    ask traders [set partner nobody]  ; unpair
    tick
    clear-links
  ]
end

to update-globals
  set sugar-demand sugar-share * (1 - salt-demand)
  set spice-demand 1 - salt-demand - sugar-demand
end

to reset-trader
  set good-held good-produced
  set no-propensity (1 - weight) * no-propensity + weight * no-payoff
  set yes-propensity (1 - weight) * yes-propensity + weight * yes-payoff
  ; set regret (1 - weight) * regret + weight * (max-series-payoff - series-payoff)
  set regret 1  ; no regret, for now
  set utility utility + series-payoff
  set series-payoff 0
  set max-series-payoff 0
  set yes-payoff 0
  set no-payoff 0
  set previous-good good-produced
  let exp-no  exp (lambda * no-propensity / regret)
  let exp-yes exp (lambda * yes-propensity / regret)
  let den exp-no + exp-yes
  ;set no-prob exp-no / den ; needed?
  set trade-probability exp-yes / den
end

; taste shock
to update-desire
  let chance random-float 1
  ifelse chance < salt-demand
  [
   set good-desired salt
  ]
  [
    ifelse salt-demand <= chance and chance < salt-demand + sugar-demand
    [
      set good-desired sugar
    ]
    [
      set good-desired spice
    ]
  ]
  if good-desired = good-held [consume-or-hold set partner self]
end

; pair self with other and vice-versa
to update-partner
  if partner = nobody
  [
    set partner one-of other traders with [partner = nobody]
    if partner != nobody
    [
      ask partner [set partner myself]
    ]
  ]
end

to enter-market
  if partner != nobody
  [
    let my-good good-held
    let partner-good [good-held] of partner
    let partner-will-trade [will-trade? my-good] of partner
    let I-will-trade will-trade? partner-good
    if partner-will-trade
    [
      update-yes-payoff
      update-no-payoff
      if I-will-trade [trade]
    ]
  ]
end

; what if I never speculate
to update-no-payoff
  if partner != nobody and good-desired = [good-held] of partner
  [
    set no-payoff no-payoff + marginal-utility
  ]
end

;what if I always speculate
to update-yes-payoff
  if partner != nobody
  [
    let partner-good [good-held] of partner
    ; this is key: would partner have traded for the good offered last tick:
    let would-have-traded [will-trade? previous-good] of partner
    if partner-good = good-desired or would-have-traded
    [
      set yes-payoff yes-payoff + marginal-utility
    ]
  ]
end

to-report will-trade? [offered-good]
  if good-desired = previous-good
  [
    set max-series-payoff max-series-payoff + 1 ; needed for regret
  ]
  set previous-good offered-good
  ifelse offered-good = good-held or offered-good = good-produced
  [
    report false ; why bother?
  ]
  [
    ifelse offered-good = good-desired
    [
      report true
    ]
    [
      ; a non-consumable good was offered to me
      set offers replace-item offered-good offers (1 + item offered-good offers)
      ; maybe I'll take it
      report random-float 1 <= trade-probability
    ]
  ]
end

to trade
  if partner != nobody
  [
    create-link-with partner   ; a bit of graphics for show
    let old-good good-held
    set good-held [good-held] of partner
    consume-or-hold
    ask partner [set good-held old-good consume-or-hold]
  ]
end

; change name?
to consume-or-hold
  ifelse good-held = good-desired
  [
    set series-payoff series-payoff + marginal-utility - production-cost
    set good-held good-produced
  ]
  [
    ; I'm holding good as money
    set money replace-item good-held money (1 + item good-held money)
  ]
end

;===========================
; metrics, utilities, etc.
;===========================

to-report total-utility [trader-type]
  let util 0
  ask traders with [good-produced = trader-type] [set util util + utility]
  report util ; / count traders with [good-produced = trader-type]
end

to-report moneyness [good]
  let num-offers item good offers
  let num-accepts item good money
  report ifelse-value num-offers = 0 [0] [num-accepts / num-offers]
end


;to update-series-utility
;  set series-utility 0
;  ask traders [set series-utility series-utility + series-payoff]
;end
;
;to-report supply [good]
;  report count traders with [good-produced = good] / count traders
;end
;
;to-report demand [good]
;  report count traders with [good-desired = good] / count traders
;end
;
;to-report distribution [produced-good held-good]
;  report count traders with [good-produced = produced-good and good-held = held-good] / count traders
;end

;to-report update-distribution [current-distribution strategies]
;  report new-distribution
;end
