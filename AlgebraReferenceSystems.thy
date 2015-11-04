theory AlgebraReferenceSystems
imports Complex_Main HOL
begin

declare [[show_types]]

--{*
An Isabelle theory describing spatio-temporal data generation. It is a supplement to the paper Modelling spatio-temporal information generation". 
The theory contains no proofs because all necessary reasoning is done on the type checking level.
Authors: Simon Scheider, Benjamin Gräler, Christoph Stasch and Edzer Pebesma
*}



--{*Basic Types*}
--{*these denote domains of reference systems*}
typedecl  Q --{*Quality*}
typedecl  S --{*Space*}
typedecl  T --{*Time*}
typedecl D --{*Discrete entities*}

consts
error :: "'a"                      --{*Error constant}
axiom 1: f error = error



--{*Defined types*}

--{*Space and time*}
type_synonym Occurs = "(S*T) set" --{*type_synonym STrelation = "(S * T) set"*}

type_synonym R = "S set" --{*Spatial Regions*}
type_synonym I = "T set" --{*Time intervals*}
type_synonym Extent = "R * I"

type_synonym SSelect = "R \<Rightarrow> S"-- {* Centroid of region*}
type_synonym TSelect = "I \<Rightarrow> T"-- {*Centroid selections of time interval*}
type_synonym Select = "Extent \<Rightarrow> S * T"-- {* Centroid of Extent*}

type_synonym STessel = "S \<Rightarrow> R"    --{*a spatial tesselation*}
type_synonym TTessel = "T \<Rightarrow> I"    --{*a temporal tesselation*}
type_synonym Tessel = "S \<Rightarrow> T \<Rightarrow> (R*I)" --{*a  tesselation*}

--{*Quality values*}
type_synonym Qs = "Q set" --{*value groups*}
type_synonym QPartition = "Q \<Rightarrow> Qs"    --{*value partition*}
type_synonym Qstat = "(Q \<Rightarrow> bool) \<Rightarrow> Q" --{* a summary statistic*}

--{*Fields*}
type_synonym Field = "S * T \<Rightarrow> Q"  --{*Spatio-Temporal field*}
type_synonym SField = "S \<Rightarrow> Q"  --{*Spatial field*}
type_synonym TField = "T \<Rightarrow> Q"  --{*Temporal field (Time series)*}

--{*Lattices*}
type_synonym LatticeS = "R \<Rightarrow> T \<Rightarrow> Q" --{*Spatio-Temporal Lattice*}
type_synonym LatticeT = "S \<Rightarrow> I \<Rightarrow> Q" --{*Spatio-Temporal Lattice 2*}
type_synonym SLattice = "R \<Rightarrow> Q"
type_synonym TLattice = "I \<Rightarrow> Q"
type_synonym Lattice = "R \<Rightarrow> I \<Rightarrow> Q"

--{*QS and QT; names need to be clarified*} 
type_synonym QS = "Q \<Rightarrow> S"
type_synonym QT = "Q \<Rightarrow> T"
type_synonym QTS = "Q \<Rightarrow> (T * S)"
type_synonym QTR = "Q \<Rightarrow> (T * R)"
type_synonym QIS = "Q \<Rightarrow> (I * S)"

--{*Coverages*}
type_synonym SInvField = "Q \<Rightarrow> R" --{*spatial inverted field*}
type_synonym TInvField = "Q \<Rightarrow> I" --{*Temporal inverted field*}
type_synonym InvField = "Q \<Rightarrow> Occurs" --{*Spatio-temporal inverted field*}

--{*Events*}
type_synonym Events = "D \<Rightarrow> (S * T)"  --{*earthquakes' epicentres*}
type_synonym RegionalEvents = "D \<Rightarrow> (R * T)"  --{*tsunamis' affected areas*}
type_synonym IntervalEvents = "D \<Rightarrow> (S * I)"  --{*volcanic eruptions*}
type_synonym BlockEvents = "D \<Rightarrow> Extent"  --{*region and duration of powerlosses*}

type_synonym SEvents = "D \<Rightarrow> S" --{*event locations*}
type_synonym REvents = "D \<Rightarrow> R" --{*violent conflict regions*}
type_synonym TEvents = "D \<Rightarrow> T" --{*event times*}
type_synonym IEvents = "D \<Rightarrow> I" --{*event intervals/time steps an object has been observed*}

type_synonym MarkedEvents = "D \<Rightarrow> (S * T * Q)"  --{*earthquakes' epicenters*}
type_synonym MarkedRegionalEvents = "D \<Rightarrow> (R * T * Q)"  --{*tsunamis' affected areas*}
type_synonym MarkedIntervalEvents = "D \<Rightarrow> (S * I * Q)"  --{*volcanic eruptions*}
type_synonym MarkedBlockEvents = "D \<Rightarrow> (Extent * Q)"  --{*region and duration of powerlosses + affected *}

type_synonym SMarkedEvents = "D \<Rightarrow> (S * Q)" --{*selection of a single time step/interval*}
type_synonym RMarkedEvents = "D \<Rightarrow> (R * Q)" --{*selection of a single time step/interval*}
type_synonym TMarkedEvents = "D \<Rightarrow> (T * Q)" --{*selection of a single location/region*}
type_synonym IMarkedEvents = "D \<Rightarrow> (I * Q)" --{*legislation periods?*}

--{*Objects*} 
type_synonym Trajectory = "T \<Rightarrow> S"  --{*trajectory, T\<Rightarrow> R?*}
type_synonym RegionalTrajectory = "T \<Rightarrow> R" --{*presence in post code*}  
type_synonym IntervalTrajectory = "I \<Rightarrow> S" --{*daily centroids*} 
type_synonym BlockTrajectory = "I \<Rightarrow> R"  --{*daily post code data*}

type_synonym Objects = "D \<Rightarrow> T \<Rightarrow> S"  --{*set of trajectories for objects*}
type_synonym RegionalObjects = "D \<Rightarrow> T \<Rightarrow> R" --{*collection of hourly regions per animal*}
type_synonym IntervalObjects = "D \<Rightarrow> I \<Rightarrow> S" --{*collection of daily centroids per animal*}
type_synonym BlockObjects = "D \<Rightarrow> I \<Rightarrow> R" --{*GI students locations per day*}

type_synonym ObjectQuality = "D \<Rightarrow> Q" --{*Quality of an object (static)*}
type_synonym ObjectTimeSeries = "D \<Rightarrow> T \<Rightarrow> Q"  --{*Object Time series*}
type_synonym ObjectIntervalTimeSeries = "D \<Rightarrow> I \<Rightarrow> Q" --{*collection of interval timeseries*} 

type_synonym SpatialObjects = "D \<Rightarrow> (S * Q)"
type_synonym TemporalObjects = "D \<Rightarrow> (T * Q)"

type_synonym MarkedTrajectory = "T \<Rightarrow> (S * Q)"  --{* uncurried = (D*T)\<Rightarrow> (S*Q); dynamic objects, forest fire?*}
type_synonym MarkedIntervalTrajectory = "I \<Rightarrow> (S * Q)"  --{*when object is implicit*}
type_synonym MarkedRegionalTrajectory = "T \<Rightarrow> (R * Q)"  --{*when object is implicit*}
type_synonym MarkedBlockTrajectory = "I \<Rightarrow> (R * Q)"  --{*when object is implicit*}

type_synonym MarkedObjects = "D \<Rightarrow> T \<Rightarrow> (S * Q)"  --{* uncurried = (D*T)\<Rightarrow> (S*Q); dynamic objects, forest fire?*}
type_synonym MarkedIntervalObjects = "D \<Rightarrow> I \<Rightarrow> (S * Q)"  --{* uncurried = (D*T)\<Rightarrow> (S*Q); dynamic objects, forest fire?*}
type_synonym MarkedRegionalObjects = "D \<Rightarrow> T \<Rightarrow> (R * Q)"  --{* uncurried = (D*T)\<Rightarrow> (S*Q); dynamic objects, forest fire?*}
type_synonym MarkedBlockObjects = "D \<Rightarrow> I \<Rightarrow> (R * Q)"  --{* uncurried = (D*T)\<Rightarrow> (S*Q); dynamic objects, forest fire?*}
 
type_synonym STObjectConstr = "Occurs \<Rightarrow>  D"

--{*Some abbreviations*}
definition tuple :: "'a \<Rightarrow> 'b \<Rightarrow> ('a*'b)" where
"tuple a b = (a, b)"
definition inters :: "'a set \<Rightarrow> 'a set \<Rightarrow> 'a set" where
"inters a b = a\<inter>b"



--{*"Primitives" of the Algebra (here defined in terms of Isabelle constructs)*}
--{*Isabelle constructs used: id,(),fst, snd, inv, Domain, Range, Union *}

definition sglton ::"'a \<Rightarrow> 'a set" where
"sglton a == {a}"

definition inv_into :: "'a set => ('a => 'b) => ('b => 'a)" where
"inv_into A f == %x. SOME y. y : A & f y = x"

abbreviation inv :: "('a => 'b) => ('b => 'a)" where
"inv == inv_into UNIV"

--{*currying and uncurrying*}
definition curry :: "('a * 'b \<Rightarrow> 'c) \<Rightarrow> ('a \<Rightarrow> 'b \<Rightarrow> 'c)" where
"curry f == \<lambda> a b. f (a, b)"
notation curry ("[\<c>]" )
definition uncurry :: "('a \<Rightarrow> 'b \<Rightarrow> 'c)\<Rightarrow> ('a * 'b \<Rightarrow> 'c)" where
"uncurry f == \<lambda> (a, b). f a b"
notation uncurry ("[\<u>\<c>]" )

--{*Sets to predicates and vice versa*}
definition ptoset:: "('a \<Rightarrow> bool) \<Rightarrow> 'a set" where
"ptoset f == {a. f a}"
notation ptoset ("[\<p>\<two>\<s>]" )
definition settop :: " ('a  set) \<Rightarrow> ('a \<Rightarrow> bool)" where
"settop s == \<lambda> a . a \<in> s"
notation settop ("[\<s>\<two>\<p>]" )

--{*combinations*}
definition comb :: "('x \<Rightarrow> 'y) \<Rightarrow> ('x \<Rightarrow> 'z) \<Rightarrow> ('x \<Rightarrow> ('y * 'z))" where 
"comb f_1  f_2  == \<lambda>x. (f_1(x),f_2(x))" 
notation comb (infixl "[\<odot>]" 100)

--{*Compositions *}
definition comp :: "('x \<Rightarrow> 'y) \<Rightarrow> ('y \<Rightarrow> 'z)\<Rightarrow> ('x \<Rightarrow> 'z)" where
"comp f_1  f_2 == \<lambda>x. f_2(f_1(x))"
notation comp (infixl "[\<circ>]" 100)

--{*Function restriction, domains, ranges and images*}
definition subfun :: "('x \<Rightarrow> 'z) \<Rightarrow>('x \<Rightarrow> bool) \<Rightarrow> ('x \<Rightarrow> 'z)" where
"subfun f p == \<lambda> x. (if (p x \<and> x \<noteq> error) then (f x) else (error))"
notation subfun (infixl "[\<s>\<u>\<b>\<f>]" 100)
definition subdom :: "('a \<Rightarrow>'b) \<Rightarrow>'b set  \<Rightarrow> 'a set" where
"subdom f b == {a. f a \<in> b}"
definition image :: "('a \<Rightarrow>'b) \<Rightarrow> 'a set \<Rightarrow> 'b set" where
"image f b == (f`(b))"

--{*Relation primitives*}
--{*Relation Images*}
definition relimage::"('a * 'b) set \<Rightarrow> 'a set \<Rightarrow> 'b set" where
"relimage rel as == (rel)``as"
--{*Cartesian products*}
definition prod :: "'a set \<Rightarrow> 'b set \<Rightarrow> ('a * 'b) set" where
"prod as bs == {(a,b). a \<in> as \<and> b \<in> bs}"
notation prod (infixl "[\<star>]" 100)
--{*Data generator*}
definition map :: "'a set \<Rightarrow> ('a \<Rightarrow> 'b) \<Rightarrow> 'b set" where
"map as f == {b. \<exists>a. a \<in> as \<and> f a = b}"
notation map (infixl "[\<rightharpoonup>]" 100)

--{*Object constructors*}
--{*generates equivalence classes of a relation*}
definition eqcl :: "'a rel \<Rightarrow> ('a set set)" where
"eqcl R == \<Union> (x::'a). {R``{x}}"
--{*object constructor from sets*}
datatype 'a Ob = obj "'a set" 
consts omap :: "'a Ob \<Rightarrow> D"
definition objident :: "'a set \<Rightarrow>  D" where
"objident as == omap(obj(as))"

--{*Quality construction*}
consts
qmap :: "D \<Rightarrow> Q"
--{*End of "Primitives"*}





--{*Algebra Definitions*}

--{*Abstract helper operations*}

definition comb2 :: "('u \<Rightarrow> 'x \<Rightarrow> 'y) \<Rightarrow> ('u \<Rightarrow> 'x \<Rightarrow> 'z) \<Rightarrow> ('u \<Rightarrow>'x \<Rightarrow> ('y * 'z))" where 
"comb2 f_1 f_2 ==  curry ((uncurry(f_1)) [\<odot>] (uncurry(f_2)))"

--{*Unbracketing, projection and permutations*}
definition unbr :: "(('a * 'b) *'c) \<Rightarrow> ('a * 'b *'c)"  where
"unbr t  == (fst (fst t), snd (fst t), snd t)"
definition scnd  where "scnd == snd [\<circ>] fst"
definition thrd  where "thrd == snd [\<circ>] snd"
definition fourth  where "fourth == snd [\<circ>] snd [\<circ>] snd"
definition switchtuple :: "('a * 'b) \<Rightarrow> ('b * 'a) " where 
"switchtuple ab == (snd ab, fst ab)"
definition switchtriple :: "('a * 'b * 'c) \<Rightarrow> ('b * 'c * 'a) " where 
"switchtriple abc == (scnd abc, thrd abc, fst abc)"
definition br :: "('a * 'b *'c) \<Rightarrow> (('a * 'b) *'c)"  where
"br  == inv unbr"

--{*switching of inputs*}
definition switch :: "('a \<Rightarrow> 'b \<Rightarrow>'c) \<Rightarrow> ('b \<Rightarrow> 'a \<Rightarrow> 'c)" where
"switch f == curry(switchtuple[\<circ>](uncurry f))"
notation switch ("[\<s>\<w>]" )

definition comp2:: "('x \<Rightarrow> 'y) \<Rightarrow> ('a \<Rightarrow> 'b)\<Rightarrow> ('y \<Rightarrow>'b \<Rightarrow> 'z)\<Rightarrow> ('x \<Rightarrow>'a \<Rightarrow> 'z)" where
"comp2 f1 f2 f == switch(f2[\<circ>](switch(f1[\<circ>]f)))"

definition ptoset2 :: "('a \<Rightarrow> 'b \<Rightarrow> bool) \<Rightarrow> (('a * 'b) set)" where
"ptoset2 f == ptoset (uncurry f)"

definition subdomproj :: "('a \<Rightarrow>'b) \<Rightarrow>'b  \<Rightarrow> 'a set" where
"subdomproj f b == subdom f (sglton b)"

--{*splits*}
definition splitl :: "('x \<Rightarrow> ('y * 'z)) \<Rightarrow> ('x \<Rightarrow> 'y)" where 
"splitl f   == f [\<circ>] fst"
definition splitr :: "('x \<Rightarrow> ('y * 'z)) \<Rightarrow> ('x \<Rightarrow> 'z)" where 
"splitr f   == f [\<circ>] snd" 

--{*writing out function inputs*}
definition out :: "('a  \<Rightarrow>'c) \<Rightarrow> ('a \<Rightarrow> ('c * 'a))" where
"out f == f [\<odot>] id"
definition out\<^sub>2\<^sub>r :: "('a \<Rightarrow>'b \<Rightarrow>'c) \<Rightarrow> ('a \<Rightarrow> 'b \<Rightarrow>  ('c * 'b))" where
"out\<^sub>2\<^sub>r f ==  f [\<circ>] out"
definition out\<^sub>2\<^sub>l :: "('a \<Rightarrow>'b \<Rightarrow>'c) \<Rightarrow> ('a \<Rightarrow> 'b \<Rightarrow>  ('c * 'a))" where
"out\<^sub>2\<^sub>l  ==   switch [\<circ>] out\<^sub>2\<^sub>r [\<circ>] switch"
definition out\<^sub>2 :: "('a \<Rightarrow>'b \<Rightarrow>'c) \<Rightarrow> ('a \<Rightarrow> 'b \<Rightarrow>  ('c * 'a * 'b))" where
"out\<^sub>2  ==  curry(curry(uncurry(uncurry (out\<^sub>2\<^sub>l [\<circ>] out\<^sub>2\<^sub>r))[\<circ>] unbr))"

definition domain :: "('a \<Rightarrow>'b)  \<Rightarrow> 'a set" where
"domain f == subdomproj f error"
definition domain2l :: "('a \<Rightarrow>'b \<Rightarrow>'c) \<Rightarrow> ('b \<Rightarrow>'a set)" where
"domain2l f == (switch f) [\<circ>] domain"
definition domain2r :: "('a \<Rightarrow>'b \<Rightarrow>'c) \<Rightarrow> ('a \<Rightarrow>'b set)" where
"domain2r f == domain2l (switch f)"

--{*Images*}
definition imageb :: "('a \<Rightarrow>'b) \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> ('b \<Rightarrow> bool)" where
"imageb f b == settop (image f (ptoset b))"

--{*aggregations *}
definition agg :: "('x0 \<Rightarrow> 'xi \<Rightarrow> 'y) \<Rightarrow>('x0 \<Rightarrow>'xi \<Rightarrow> bool)\<Rightarrow> (('y \<Rightarrow> bool) \<Rightarrow> 'y) \<Rightarrow> 'y" where
"agg f p n == n (imageb (uncurry f) (uncurry p))"
definition agg\<^sub>l :: "('x0 \<Rightarrow> 'xi \<Rightarrow> 'y) \<Rightarrow>('x0 \<Rightarrow> bool)\<Rightarrow> (('y \<Rightarrow> bool) \<Rightarrow> 'y) \<Rightarrow> ('xi \<Rightarrow>'y)" where
"agg\<^sub>l f p n ==  ((switch((switch f) [\<circ>] imageb)) p) [\<circ>] n" 
definition agg\<^sub>r :: "('x0 \<Rightarrow> 'xi \<Rightarrow> 'y) \<Rightarrow>('xi \<Rightarrow> bool)\<Rightarrow> (('y \<Rightarrow> bool) \<Rightarrow> 'y) \<Rightarrow> ('x0\<Rightarrow>'y)" where
"agg\<^sub>r f p n ==  agg\<^sub>l (switch f) p n"
--{*d in the following is a data set of the domain over which it is aggregated*}
definition aggT :: "('x0 \<Rightarrow> 'xi \<Rightarrow> 'y) \<Rightarrow> ('x0 \<Rightarrow>'xi \<Rightarrow> 'A) \<Rightarrow> (('y \<Rightarrow> bool) \<Rightarrow> 'y)  \<Rightarrow> ('x0 \<Rightarrow>'xi \<Rightarrow> bool)  \<Rightarrow> ('A \<Rightarrow> 'y)" where
"aggT f r n d == (subdomproj (uncurry r))[\<circ>] (inters (ptoset (uncurry d))) [\<circ>] ((switch map) (uncurry f))[\<circ>] settop [\<circ>] n"
definition aggTr :: "('x0 \<Rightarrow> 'xi \<Rightarrow> 'y) \<Rightarrow> ('xi \<Rightarrow> 'A) \<Rightarrow> (('y \<Rightarrow> bool) \<Rightarrow> 'y) \<Rightarrow> ('xi  \<Rightarrow> bool) \<Rightarrow> ('x0 \<Rightarrow> 'A \<Rightarrow> 'y)" where
"aggTr f r n d == curry ((uncurry (f[\<circ>](comp2 id ((subdomproj r) [\<circ>] (inters (ptoset d))) (switch map))))[\<circ>] settop [\<circ>] n)  "
definition aggTl :: "('x0 \<Rightarrow> 'xi \<Rightarrow> 'y) \<Rightarrow> ('x0 \<Rightarrow> 'A) \<Rightarrow> (('y \<Rightarrow> bool) \<Rightarrow> 'y) \<Rightarrow>('x0  \<Rightarrow> bool) \<Rightarrow>  ('A \<Rightarrow>'xi \<Rightarrow> 'y)" where
"aggTl f r n d == switch (aggTr (switch f) r n d)"


--{*Function inversion (inv)*}
definition invb :: "('x \<Rightarrow> 'z) \<Rightarrow>('x \<Rightarrow> bool) \<Rightarrow> ('z \<Rightarrow> 'x)" where
"invb f p == inv (subfun f p) "
definition invlocf :: "('x \<Rightarrow> 'y \<Rightarrow> 'z) \<Rightarrow> ('x \<Rightarrow> bool)\<Rightarrow> ('z \<Rightarrow> 'y \<Rightarrow> 'x)" where
"invlocf f p == switch((switch((switch f) [\<circ>] invb)) p)"

--{*Relations*}
definition relimageproj :: "('a * 'b) set \<Rightarrow> 'a \<Rightarrow> 'b set" where
"relimageproj rel b == relimage rel (sglton b)"
definition relsubdom :: "('a * 'b) set \<Rightarrow> 'b \<Rightarrow> 'a set" where
"relsubdom rel b == relimageproj (map rel switchtuple) b"

--{*Cartesian products*}
definition cartpr :: " ('a set) \<Rightarrow> ('b set) \<Rightarrow> ('a \<Rightarrow> 'b \<Rightarrow> bool)" where
"cartpr as bs == curry(settop (prod as bs))"

--{*data generators*}
definition gendata :: "'a set \<Rightarrow>('a \<Rightarrow> 'b) \<Rightarrow> ('a * 'b) set" where
"gendata as f ==map as ((out f) [\<circ>] switchtuple)"

--{*object constructors*}

--{*General object constructor from equivalence relations*}
definition objground :: "D  \<Rightarrow> 'a set" where
"objground  = inv objident"
definition objcons ::  "('a set) set \<Rightarrow> D set" where
"objcons s == map s objident"

--{*object constructor from arbitrary relations*}
definition genObjfromrel ::"'a rel \<Rightarrow> D set" where
"genObjfromrel R == objcons( eqcl( R^* ))"

definition consObjOccurs ::"('a \<Rightarrow> S * T) \<Rightarrow> D \<Rightarrow> Occurs" where
"consObjOccurs w  ==  objground [\<circ>] ((switch map) w)"
--{*where type 'a is the type in which the object is defined (i.e based on which it was constructed) and there is a mapping from space time quality into this type*}

definition consObjFromField :: "Field \<Rightarrow>(Field \<Rightarrow> ((S*T) \<Rightarrow>(S*T) \<Rightarrow> bool)) \<Rightarrow> D set" where
"consObjFromField f torel == genObjfromrel(ptoset(uncurry(torel f)))"

--{*End of abstract helper operations*}




--{*Derivations of spatio-temporal information*}

--{*Start types (primitives): Field, Qstat, SSelect, getObjOccurs, STessel, TTessel,
Q, S, T, D  *}

--{*Derive trajectories from occurrences*}
definition regTrajfromOccs :: "(S * T) set \<Rightarrow> RegionalTrajectory" where
"regTrajfromOccs R b == relimage (map R switchtuple) (sglton b)"

--{*Derive object trajectories from  occurrences and a spatial selection (e.g. centroid) *}
definition trajfromOccs :: " SSelect \<Rightarrow> Occurs \<Rightarrow> Trajectory" where
"trajfromOccs sel P  == (regTrajfromOccs P)[\<circ>] sel"
definition objectTrajfromOccs :: " SSelect \<Rightarrow> (D \<Rightarrow> Occurs) \<Rightarrow>  Objects" where
"objectTrajfromOccs sel obd  == (obd [\<circ>] (trajfromOccs  sel))"

--{*Synthesize dynamic objects*}
definition synthdynamicobjects :: "Objects \<Rightarrow>  ObjectTimeSeries \<Rightarrow>  MarkedObjects" where 
 "synthdynamicobjects otr ots ==  comb2 otr ots"
definition synthdynamicobject :: "RegionalTrajectory \<Rightarrow>  TField \<Rightarrow> MarkedRegionalTrajectory" where 
 "synthdynamicobject tr ts ==  comb tr ts"
definition synthdynamicRobjects :: "RegionalObjects \<Rightarrow>  ObjectTimeSeries \<Rightarrow>  MarkedRegionalObjects" where 
 "synthdynamicRobjects otr ots ==  comb2 otr ots"

--{*Specialisations of fields*}
definition deriveTfield :: "Field \<Rightarrow>  S \<Rightarrow>  TField" where 
"deriveTfield field s ==  (curry field) s"
definition deriveSfield :: "Field \<Rightarrow>  T \<Rightarrow>  SField" where 
"deriveSfield field t ==   switch(curry field) t"

--{*Aggregations of fields*}
--{*Note that d always stands here for the sample of points taken from the continuous field as a data basis for aggregation*}
definition spatialAgg :: "Field \<Rightarrow>  STessel \<Rightarrow>  Qstat \<Rightarrow> S set \<Rightarrow>  LatticeS" where
"spatialAgg field st qs d == aggTl (curry field) st qs (settop d)"
definition temporalAgg :: "Field \<Rightarrow>  TTessel \<Rightarrow>  Qstat \<Rightarrow> T set \<Rightarrow>   LatticeT" where
"temporalAgg field tt qs d == aggTr (curry field) tt qs (settop d)"
definition sptAgg :: "Field \<Rightarrow>  Tessel  \<Rightarrow>  Qstat \<Rightarrow> Occurs \<Rightarrow> Lattice" where
"sptAgg field tes qs d == curry (aggT (curry field) tes qs (curry (settop d)))"
definition thematicagg\<^sub>rS :: "SField  \<Rightarrow>  (Q  \<Rightarrow>  bool)  \<Rightarrow>  R" where "thematicagg\<^sub>rS f p ==   ptoset (f [\<circ>] p)"
definition thematicagg\<^sub>r :: "Field  \<Rightarrow>  (Q  \<Rightarrow>  bool)  \<Rightarrow>  Occurs" where "thematicagg\<^sub>r f p ==   ptoset2 (curry (f [\<circ>] p))"
definition spatialAgginR :: "Field \<Rightarrow>  R \<Rightarrow>  Qstat \<Rightarrow> S set \<Rightarrow> TField" where
"spatialAgginR f r qs d == agg\<^sub>l (curry f) (settop (inters r d)) qs"
definition sptAgginRI :: "Field \<Rightarrow>  Extent \<Rightarrow>  Qstat \<Rightarrow> Occurs \<Rightarrow>  Q" where
"sptAgginRI f ri qs d == agg (curry f) (curry(settop (inters (prod (fst ri) (snd ri)) d))) qs"

--{*Simple Derivations from objects*}
definition do2ot :: "MarkedObjects \<Rightarrow>  Objects" where
"do2ot do == curry (splitl (uncurry do))"
definition do2ots :: "MarkedObjects \<Rightarrow>  ObjectTimeSeries" where
"do2ots do == curry (splitr (uncurry do))"
definition ot2t :: "Objects \<Rightarrow>  D \<Rightarrow>  Trajectory" where
"ot2t do d ==  do d"
definition do2dro :: "MarkedObjects \<Rightarrow>  STessel \<Rightarrow>  MarkedRegionalObjects" where
"do2dro do st == curry (comb ((uncurry (do2ot do))[\<circ>] st) (uncurry(do2ots do)))"

--{*Derive fields from objects*} 
definition exfieldfromobjects :: "Objects \<Rightarrow> (D set) \<Rightarrow> Field" where
"exfieldfromobjects ot ds  ==  (uncurry(invlocf ot (settop ds)))[\<circ>] qmap"
definition fieldfromobjects :: "MarkedObjects \<Rightarrow> (D set) \<Rightarrow>  Field" where
"fieldfromobjects  do ds  ==  (uncurry(out\<^sub>2\<^sub>r (invlocf (curry (splitl (uncurry do))) (settop ds))) )[\<circ>] (splitr (uncurry do))"

--{*Derive occurrences from objects*}
definition getOccsfromSTObj :: "D \<Rightarrow> Occurs"  where
"getOccsfromSTObj == consObjOccurs id"
definition getOccsfromTSObj :: "D \<Rightarrow> Occurs" where
"getOccsfromTSObj  == consObjOccurs switchtuple"
definition gettime :: "D \<Rightarrow> T set" where "gettime ==   (switch(getOccsfromSTObj [\<circ>] map) snd) "
definition getObjData :: "(D \<Rightarrow> T \<Rightarrow> 'a) \<Rightarrow> (D \<Rightarrow> (T *'a) set)" where
"getObjData f d == ((gettime [\<circ>] map)) d (out(f d)[\<circ>] switchtuple)"

definition extentfromOcc :: "Occurs \<Rightarrow>  Extent" where
"extentfromOcc oe == (Domain oe, Range oe)"

--{*Derive events from objects*}
--{*...from trajectories and time series . These generate ``lifetime'' temporal events from object data*}
definition ot2otimes :: "Objects  \<Rightarrow>  IEvents " where
"ot2otimes ot ==  domain2r ot" 
definition ort2otimes :: "RegionalObjects  \<Rightarrow>  IEvents " where
"ort2otimes ot == domain2r ot"
definition oit2otimes :: "IntervalObjects  \<Rightarrow>  IEvents " where
"oit2otimes ot ==  (domain2r ot) [\<circ>] Union"
definition obt2otimes :: "BlockObjects  \<Rightarrow>  IEvents " where
"obt2otimes ot == (domain2r ot) [\<circ>] Union"
definition ots2otimes :: "ObjectTimeSeries  \<Rightarrow>  IEvents " where
"ots2otimes ot == domain2r ot" 
definition oits2otimes :: "ObjectIntervalTimeSeries  \<Rightarrow>  IEvents " where
"oits2otimes ot == (domain2r ot)[\<circ>] Union" 

--{*Events from object constructors*}
definition consEvfromTSObj ::"BlockEvents" where
"consEvfromTSObj ==  getOccsfromTSObj [\<circ>] extentfromOcc"
definition consEvfromSTObj ::"BlockEvents" where
"consEvfromSTObj == getOccsfromSTObj [\<circ>] extentfromOcc"

--{*Derive trajectories from object constructors*}
definition consRTrajfromSTObj :: "RegionalObjects" where
"consRTrajfromSTObj  ==  getOccsfromSTObj [\<circ>] regTrajfromOccs "
definition gettrajdata ::"D \<Rightarrow> (T*R) set" where
"gettrajdata  ==   getObjData (consRTrajfromSTObj)"

--{*End of derivations of spatio-temporal information *}







--{*Application  examples*}


consts
f:: Field
time:: T
locations:: "S set"
latticeregions :: "R set"
stessel :: STessel
mean :: Qstat
centroid:: SSelect

--{*time series*}
consts
f_CO_2 :: Field
l_Mauna_Loa :: S
times :: "T set"

definition timeseries_Mauna_Loa :: "(T*Q) set" where
"timeseries_Mauna_Loa == gendata times  (deriveTfield f_CO_2 l_Mauna_Loa) "

--{*summer temperature of a city*}
consts
f_temp ::Field
city :: R
summer :: I
measures :: Occurs


definition avgtemp_citysummer  :: "Q" where
"avgtemp_citysummer == sptAgginRI f_temp (city,summer) mean measures"

definition avgtemp_city  :: "TField" where
"avgtemp_city == spatialAgginR f_temp city mean locations"

--{*Temperature lattice*}
consts 
 countries :: STessel
 years :: TTessel
period :: I


 definition templattice :: "Lattice" where
 "templattice == sptAgg f_temp (comp2 countries years tuple) mean measures"
definition tempSlattice :: "SLattice" where
"tempSlattice == (switch templattice) period"
definition templatticepointdata :: "(S * Q) set" where
"templatticepointdata == map latticeregions ((tempSlattice [\<odot>] centroid) [\<circ>] switchtuple)  "


--{*Hut destruction*}
consts
satimages :: Field
hutsrel :: "((S*T)*(S*T)) set"

definition huts :: "D set" where
"huts = genObjfromrel hutsrel"
definition  hutevents ::"(D*R*I) set"  where
"hutevents == gendata huts consEvfromSTObj"


--{*EnviroCar: Stop event construction*}
consts
cartrajectory :: "Trajectory"
fixs :: "T set"
withinstop :: "(T*S) \<Rightarrow>(T*S) \<Rightarrow> bool" 

definition measuretrack :: "Trajectory \<Rightarrow> T set \<Rightarrow> (T*S) set" where
"measuretrack traj ts == map ts ((out  traj)[\<circ>] switchtuple)"

definition cartrack :: "(T*S) set" where
"cartrack == measuretrack cartrajectory fixs"

definition trackrel :: "((T*S)*(T*S)) set" where
"trackrel == inters (prod cartrack cartrack) (ptoset(uncurry(withinstop)))"

definition stops :: "D set" where
"stops == genObjfromrel trackrel"

definition  stopevents :: "(D*R*I) set"  where
"stopevents == gendata stops  consEvfromTSObj"


--{*EnviroCar: Generate Space-time cubes for a selection of cars*}
consts
tracks :: "Objects"
omeasures :: "ObjectTimeSeries"
coarsecubes :: Tessel
cars :: "D set"
card :: "'a set \<Rightarrow> Q"

definition spaceTimeCube :: Field where 
"spaceTimeCube == exfieldfromobjects tracks cars"
--{*EnviroCar: This cube shows a measured quality for all cars*}
definition spaceTimeCube2 :: Field where
"spaceTimeCube2 == fieldfromobjects (curry (comb (uncurry tracks) (uncurry omeasures))) cars"


definition getOccsfromst :: "D  \<Rightarrow> Occurs"  where "getOccsfromst ==  inv objident"

--{*Forest fires (trajectories from field)*}
consts 
satimages2:: Field
consRelFromField :: "Field \<Rightarrow> ((S*T) \<Rightarrow>(S*T) \<Rightarrow> bool)" 

--{*Fire constructor*}
definition fires :: "D set" where
"fires == consObjFromField satimages2 consRelFromField" 

--{*Fire as event*}
definition fireevents ::"(D*R*I) set" where
"fireevents == gendata fires consEvfromSTObj"

--{*Fire as object trajectory*}
definition firetrajectories :: "(D*((T*R) set)) set" where
"firetrajectories == gendata fires gettrajdata"



--{*End of application  examples*}

end
