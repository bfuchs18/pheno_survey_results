This repo contains the code to generate tables and plots of [SFARI Phenotypic Data – Researcher Feedback Survey](https://docs.google.com/forms/d/1W0RI6o4_44GvYoOeFR_uQNEljX4UmxWxj20D7G-Nwr0/edit)

Files: 

- [science_meeting.Rmd](science_meeting.Rmd): generates tables and charts presented during the Science Meeting ([7.2.2025](https://docs.google.com/presentation/d/1jYTZHAtp3TzXjpyajUk3LD0btHpi6Qja6UmNbL2886M/edit?slide=id.p#slide=id.p)
- [cdwg_survey_results.Rmd](cdwg_survey_results.Rmd): generates tables and charts presented during the CDWG ([5.12.25](https://docs.google.com/presentation/d/1Cdr-MK6GBPRdR7o8XaTI561gqUR5Hlrwf3TOY3nJptM/edit?slide=id.p#slide=id.p)
- [scripts/setup.R](scripts/setup.R): cleans data, sourced by Rmds

To execute either .Rmd:

- Individuals within the Simons Foundation can download data as a CSV from [here](https://docs.google.com/spreadsheets/d/17yJm2FvUAkoavEbzQdsrmexj1e6ahE69EeNbId2yGeI/edit?resourcekey=&gid=2039702603#gid=2039702603)
- Place the CSV into [/data](data)
- In the "setup" Rmd chunk, set the name of responses_file in setup() -- e.g., setup(responses_file = "data/responses_2025-05-11.csv")
- Execute the .Rmd
