--Return of each asset
assetR :: Position
assetR = [0.09,0.03, 0.7, 0.1,0.01]

--Expected return of portfolio
expR :: Position
expR = [0.0091,0.0090, 0.0107, 0.0041,0.0118]

wpg2 :: (Double,Double,Double)
wpg2 = (0.7,1.45,1.49)


A 0.01 0.0091
B 0.0092 0.0090
C 0.017 0.0107
D 0.0041 0.0041
E 0.012 0.0118
F 0.007 0.001
G 0.01 0.007
H 0.032 0.03
I 0.005 0.003
J 0.03 0.015

% When the difference between investing well and investing efficiently results in not gains hundreds of thousands of pounds, it becomes quite obvious that 

% at finding the maxima or minimia of given problems over specified domains, its special success is at not becoming trapped in local maxima or minimas even though the algorithm might encounter multiple of them before finding the global maxima or minima due to the concept of randomness being introduces, not only to each particle but also to the initialisation of the whole swarm itself. When evolutionary algorithms come across functions, such as $f(x)=\sum\limits_{i=1}^n -x_i sin(\sqrt{|x_i|})$ which was considered \cite{localmin} as a particularly hard function to optimise due to the number of local minima increases exponentially with the number of dimensions of the searching space, the danger might be that when it finds a certain local minima which might seem like a ``good enough'' solution, then the algorithm might converge to this false solution. Furthermore when what we're trying to optimise has local minima and global minima which differ by thousands of pounds worth of stocks when applied to financial situations it is crucial the algorithm does not get stuck in non-optimal solutions. 


Hill climbing / greedy search - simplex - scalability problem on deterministic. Easy for hill climbing to get stuck in local opt

Background, don't mention where I learnt it from, mentions what the chapter is about and what is said there. 

Improve chapter intros. "In this chapter blah blah blah"

Relationships = parameter investigation, np vs nit for results
----------------------------------------------------------------------------------



Scalability, running time, cuality number of assets. YES IMPORTANT

PSO, risk measure, results and conclusion for presentation. User more visual stuff. Focus on adaptation and results. Scalability. 

Did you compare PSO with baseline. Solving port using PSO, not testing PSO,. WAnt to use PSO in real-world application, that's IMPORTANT
Mention amount of work lots of test cases with lots of tests,
How did you develop fitness function, penalty etc. 
Furture real-time process, compare Multi-objective vs penalty.
PSO in financlial problem so might be able to apply to other financial stuff. 