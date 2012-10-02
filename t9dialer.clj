(ns t9dialer)

(def keyboard
  {
   1 [] ; FIXME
   2 [\a \b \c]
   3 [\d \e \f]
   4 [\g \h \i]
   5 [\j \k \l]
   6 [\m \n \o]
   7 [\p \q \r \s]
   8 [\t \u \v]
   9 [\w \x \y \z]
  })

(def test-db
  ["Ana", "Gigel", "Dorel", "Maria", "Alex", "Cristi", "Andrei", "Diana"])



(defn str-matches? [s numbers]
  "Checks if the string (s) matches the t9 input of the
  vector of digits (numbers)"
  (letfn
    [(str-matches-rec [s numbers]
      "Recursively checks if the first char in the str matches
      one of the chars on the first given number key"
      (if (empty? numbers)
        true
        (let [curr-num (first numbers)
              vars (keyboard curr-num)
              curr-char (first s)]
          (and (reduce #(or %1 %2) (map #(= % curr-char) vars))
               (recur (rest s) (rest numbers))))))]
    (let [s (seq s), nums (seq numbers)]
      (if (> (count nums) (count s))
        false
        (str-matches-rec s nums)))))

(defn get-matches [db numbers]
  "Given a vectof of strings and a vector of digits, return those strings
  which match the t9 dial given by the given digits"
  (filter #(str-matches? (.toLowerCase %) numbers) db))
