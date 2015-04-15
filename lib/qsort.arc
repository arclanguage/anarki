(def qsort (lst)
     (let helper (afn (start end)
                      (if (>= start end)
                          lst
                        (let pivot-pos (partition lst start end)
                             (self start
                                   (- pivot-pos 1))
                             (self (+ pivot-pos 1)
                                   end))))
          (helper 0 (- (len lst) 1)))
     lst)


(def partition (lst (o start 0) (o end (- (len lst) 1)))
     "Partitions a list in-place. 
      This method returns the position the pivot moved to."
     (withs (pivot lst.start
             higher-num-out-of-place (+ start 1)
             lower-num-out-of-place end)
            (until (is higher-num-out-of-place
                       lower-num-out-of-place)
                   (until (or (> lst.higher-num-out-of-place
                                 pivot)
                              (is higher-num-out-of-place
                                  lower-num-out-of-place))
                          (++ higher-num-out-of-place))
                   (until (or (< lst.lower-num-out-of-place
                                 pivot)
                              (is higher-num-out-of-place
                                  lower-num-out-of-place))
                          (-- lower-num-out-of-place))
                   (unless (is higher-num-out-of-place
                               lower-num-out-of-place)
                     (swap lst.higher-num-out-of-place
                           lst.lower-num-out-of-place)))
            (let pivot-end (if (> lst.higher-num-out-of-place
                                  pivot)
                               (- higher-num-out-of-place
                                  1)
                             higher-num-out-of-place)
                 (swap lst.start
                       lst.pivot-end)
                 pivot-end)))
