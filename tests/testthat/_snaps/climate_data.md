# .biosim_generate_weather errors after exhausting max_attempts

    Code
      suppressMessages(.biosim_generate_weather(modelNames = "X", fromYr = 2000,
        toYr = 2000))
    Condition
      Error:
      ! BioSIM::generateWeather() failed after 2 attempts: BioSIM web API unavailable

