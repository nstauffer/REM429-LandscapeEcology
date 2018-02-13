# LAB 3: VDDT
## Intro

This landscape being modeled has the potential for grassland, shrubland, and woodland vegetation communities. They're not discrete, and some communities exist in combinations of those three categories. EXPAND

The model is a distributional one, ignoring the spatial interactions of the communities. The purpose is to forecast what communities may exist an in what relative proportions under different management regimes.

Assuming that the predicted response pathways for the disturbances map well to reality, then this can help us to test out management approaches without actually altering the landscape and over timescales that extend far beyond the period of time we have to make decisions (simulated centuries in real time minutes).

## Methods
Flailing blindly at parameters for cutting and burning for a little while to see what happened. Leaning on the disturbance parameters that fed into pathways which led to GrFRB and ArVth communities.

First, I targeted the early juniper community E for partial cutting (and later for clear cutting and burning), but that caused a massive spike in community C well beyond the 20% I wanted.

After an extended period attempting to tune non-wildfire parameters, I turned to adjusting the age of the cells that the management actions could occur on. Although that helped, allowing me to prevent controlled burns in time windows during which a vegetation community was already slightly below my target proportion, it still proved inadequate.

## Results
~table of landscape composition at 0, 25, 50, and 100 without management~

~figure of abundance of classes B, C, F, and G up to timestep 100 without management~

~table of landscape composition at 0, 25, 50, and 100 with management~

~figure of abundance of classes B, C, F, and G up to timestep 100 with management~

Narrate the damn things

## Discussion
Why would we consider active management? Why these results without management? (hint: fire suppression policy)
Given the system as modeled with the intial parameters for landscape allocation and non-management disturbance, the outcome is that by timestep 100 the landscape becomes dominated by juniper woodland communities, which is considered undesirable as a management conclusion. Without direct management, the model has infrequent (probability ranging from 0.005 to 0.01) wildfires, which in earlier successional stages lead back to grassland and shrubland, but in later stages only to either juniper woodlands or open herbland with a circuitous route to the desired shrubland communities.

The management regime that I proposed is imperfect and results in the **Artemesia** community proportions oscillating around and overshooting the goal of 0.2. It is, however, a dramatic improvement over the "unmanaged" scenario. 

How is my scenario similar to and different from the natural disturbance regime? (HINT: is "no management" the same things as natural disturbance regime?)

What are the assumptions and limitations of this model that need to be kept in mind during interpretation?
## Literature Cited
(NEED AT LEAST TWO TO SHOW UP IN INTRO AND DISCUSSION)