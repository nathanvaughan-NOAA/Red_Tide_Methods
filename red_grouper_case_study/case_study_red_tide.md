

# SSMSE Case Study - Red Tide

This Quarto document contains the general ideas and set-up for an SSMSE
project. The goal is to make this study replicable and organized.

## General Variables

We need to specify the number of iterations and forecast years. In this
case study we will run 5 iterations and 10 forecast years.

## Operating Models

The Operating Models (OMs) are the base models of the SSMSE comparisons.
Additional OMs are base models with slight variations, for example
changes in steepness.

1.  Control - no effect of red tide (remove fleet 5?)

2.  Red tide causes episodic mortality (the current model?)

3.  Red tide causes changes in catchability (remove fleet 5 and change
    catchability of all other fleets?)

The first OM is the base Stock Synthesis model for a given species.

The following OMs are manipulated versions of the base model, either via
develop_OMs() or through methods specified in the future_om_list.

## Uncertainties

The uncertainties allow for more realistic characterization of
uncertainty in the model. These can include:

1.  Varying Magnitude of recreational catches (MRIP FES vs. biased high)
2.  Varying bias in the Ecospace red tide index (unbiased vs. biased in
    certain years)
3.  Varying coefficient of variability (CVs) on recreational catches
    (low vs. high)

Uncertainties are incorporated by creating models with slight variations
in the future_om_list. The same random numbers are used across the OM
scenarios to ensure differences in performance are from the management
strategies and not random choice.

## Management Strategies or Estimated Models

SSMSE runs can compare estimation models or management strategies. In
this project we want to test some alternative methods for varying
episodic red tide mortality so we aren’t testing management strategies.

1.  Pseudo fleet with flat selectivity across ages (current SS model)
2.  Pseudo fleet with age-varying selectivity informed by the Ecospace
    model - would need to add selectivity to fleet 5
3.  Annual blocks on natural mortality - would need to add blocking, and
    a red tide mortality index?
4.  Annual blocks on catchability - would need to add blocking, and
    catchability modifier?

## Performance Metrics

### Long-term

1.  Catch by year - TS
2.  Standard deviation of catch across years

### Short-term

1.  Terminal year depletion
2.  Average depletion over last 5 years
