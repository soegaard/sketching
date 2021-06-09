#lang racket/base
(require racket/date
         "parameters.rkt")

(provide year month day hour minute second
         millis)

(define (year) ; the year
  (define now (current-date))
  (date-year now))

(define (month) ; month in the year from 1 to 12
  (define now (current-date))
  (date-month now))

(define (day) ; day in the month, from 1 to 31
  (define now (current-date))
  (date-day now))

(define (hour) ; hour in the day from 0 to 23
  (define now (current-date))
  (date-hour now))

(define (minute) ; minute in the hour, from 0 to 59
  (define now (current-date))
  (date-minute now))

(define (second) ; second in the minute, from 0 to 59
  (define now (current-date))
  (date-second now))

(define (millis)
  (- (current-milliseconds) milliseconds-at-start-of-program))
