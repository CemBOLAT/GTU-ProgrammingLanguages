; initialize x=12, execute instructions, 
; increment it by 3 and perform iteration until it is less than 25
(loop for x from 42 downto 25 by 3 do ;>= 
	(print x) 
) 
(write-line "")

(loop for x from 42 downto (+ 25 1) by 1 do ;> 
    (print x) 
)

(loop for x from 25 to 42 by 3 do ;<= 
    (print x) 
)

(loop for x from 25 to (- 42 1) by 3 do ;< 
    (print x) 
)