#lang sicp
; ex1.26
; replacing square x with direct multiplicaton (* x x) will cause this process to not take logn but n.
; this is because previously calculating of x was done once, now it has to be done twice. and so doubling the time
; so previously with fermat we managed to cut the time by half which means we go from log speed to n speed
; now by using (* x x) we go back to where we were and lose the previously gained efficiency
