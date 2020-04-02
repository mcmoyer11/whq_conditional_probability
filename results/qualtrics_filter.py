import csv
import re

pilot_results = './processed_results.csv'
results_filtered = './stakes_past.txt'
report_metadata = './report_meta_fin.csv'

with open(results_filtered, 'rb+') as results_filtered, open(pilot_results,'wb+') as pilot_results, open(report_metadata,'rb+') as report_metadata:

    results_filtered = list(csv.reader(results_filtered))

    pilot_results = csv.writer(pilot_results)
    pilot_results.writerow(['subject','native_lang','age','gender','trial','story','reaction_time','response_a','response_b','response_c','response_d','report_a','report_b','report_c','tag_a','tag_b','tag_c','exhaustive_seen','mention_one_high_seen','mention_one_low_seen','mention_some_high_seen','mention_some_low_seen','false_report_seen','none_of_above_seen','exhaustive_chosen','mention_one_high_chosen','mention_one_low_chosen','mention_some_high_chosen','mention_some_low_chosen','false_report_chosen','none_of_above_chosen'])

    report_metadata = list(csv.reader(report_metadata,quotechar='\''))

    # filter out metadata
    results_filtered = [x for x in results_filtered if not x[0].startswith('#')]
    # filter out the dummy checkbox reports
    results_filtered = [[x[-4]]+x[-2:] for x in results_filtered]
    results_filtered = [x for x in results_filtered if x[1] != 'dummy_checkbox_']
    results_filtered = [x for x in results_filtered if x[1] != 'consent_checkbox_']
    results_filtered = [x for x in results_filtered if x[0] != 'instructions']

    subject_counter = 0
    subj_info = []
    by_trial_info = []




    for i,r in enumerate(results_filtered):
        # print r
        trial_type = r[-3] 
        probe = r[-2]
        target = r[-1]

        #subject info
        if probe == '_REACTION_TIME_':
            if trial_type == 'consent':

                # when running on MTurk, uncomment the following line
                subject = results_filtered[i-4][-1]

                # When running without MTurk IDs, uncomment th following 4 lines
                # check = subject_counter
                # subject_counter += 1
                # if subject_counter > check:
                #     subject = subject_counter

                #Morgan added the 'native_lang' stuff but also changed the indexing. 
                #I also added 'native_lang' in several places below
                native_lang = results_filtered[i-3][-1]
                age = results_filtered[i-2][-1]
                gender = results_filtered[i-1][-1]
                trial = trial_type
                # story == trial_type + respondent prefix
                story = 'NA'
                reaction_time = target
                response_a,response_b,response_c,response_d,report_a,report_b,report_c,tag_a,tag_b,tag_c = ['NA']*10
                # 'exhaustive_seen', 'mention_one_high_seen', 'mention_one_low_seen', 'mention_some_high_seen', 'mention_some_low_seen', 'false_report_seen', 'none_of_above_seen'
                exhaustive = 'NA'
                mention_one_high = 'NA'
                mention_one_low = 'NA'
                mention_some_high = 'NA'
                mention_some_low = 'NA'
                false_report = 'NA'
                none_of_above = 'NA'

                exhaustive_chosen = 'NA'
                mention_one_high_chosen = 'NA'
                mention_one_low_chosen = 'NA'
                mention_some_high_chosen = 'NA'
                mention_some_low_chosen = 'NA'
                false_report_chosen = 'NA'
                none_of_above_chosen = 'NA'

                #native_lang added here
                dataline = [subject,native_lang,age,gender,trial,story,reaction_time,response_a,response_b,response_c,response_d,report_a,report_b,report_c,tag_a,tag_b,tag_c,exhaustive,
                            mention_one_high,mention_one_low,mention_some_high,mention_some_low,false_report,none_of_above,exhaustive_chosen,mention_one_high_chosen,
                            mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen,]
                print dataline
                pilot_results.writerow(dataline)

            if trial_type == 'high' or trial_type == 'low':
                trial = trial_type
                # story == trial_type + respondent prefix
                story = trial_type + '_' + results_filtered[i-4][-2].split('_')[0]
                reaction_time = target

                response_a = results_filtered[i-4][-1]
                response_b = results_filtered[i-3][-1]
                response_c = results_filtered[i-2][-1]
                response_d = results_filtered[i-1][-1]


                response_a = convert_to_binary(response_a)
                response_b = convert_to_binary(response_b)
                response_c = convert_to_binary(response_c)
                response_d = convert_to_binary(response_d)

                answers_reported = results_filtered[i-5][-1]

                answers_reported = answers_reported.replace('placeholder','')
                answers_reported = answers_reported.replace('%2C',',')
                answers_reported = answers_reported.replace('says,', 'says:')
                answers_reported = re.findall('"([^"]*)"', answers_reported)

                report_a,report_b,report_c = answers_reported

                tags = []
                for answer in answers_reported:
                    for row in report_metadata[1:]:
                        if row[1] == answer:
                            tags.append(row[-1])
                #print(type(tags))
                #print(tags)

                tag_a, tag_b, tag_c = tags

                exhaustive = 1 if 'exh' in tags else 0
                mention_one_high = 1 if 'mo-h' in tags else 0
                mention_one_low = 1 if 'mo-l' in tags else 0
                mention_some_high = 1 if 'ms-h' in tags else 0
                mention_some_low = 1 if 'ms-l' in tags else 0
                false_report = 1 if 'f' in tags else 0
                none_of_above = 1

                tags_dict = {
                    0 :response_a,
                    1 :response_b,
                    2 :response_c
                }

                exhaustive_chosen = 1 if 'exh' in tags and tags_dict[tags.index('exh')] == 1 else 0
                mention_one_high_chosen = 1 if 'mo-h' in tags and tags_dict[tags.index('mo-h')] == 1 else 0
                mention_one_low_chosen = 1 if 'mo-l' in tags and tags_dict[tags.index('mo-l')] == 1 else 0
                mention_some_high_chosen = 1 if 'ms-h' in tags and tags_dict[tags.index('ms-h')] == 1 else 0
                mention_some_low_chosen = 1 if 'ms-l' in tags and tags_dict[tags.index('ms-l')] == 1 else 0
                false_report_chosen = 1 if 'f' in tags and tags_dict[tags.index('f')] == 1 else 0

                none_of_above_chosen = response_d

                #native_lang added here
                dataline = [subject, native_lang, age, gender, trial, story, reaction_time, response_a, response_b, response_c,
                            response_d, report_a, report_b, report_c, tag_a, tag_b, tag_c,exhaustive,mention_one_high,mention_one_low,
                            mention_some_high,mention_some_low,false_report,none_of_above,exhaustive_chosen,mention_one_high_chosen,
                            mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen]
                print dataline
                pilot_results.writerow(dataline)

            if trial_type == 'control' or trial_type == 'train':
                trial = trial_type
                # story == trial_type + respondent prefix
                if trial_type == 'control':
                    story = trial_type + '_' + results_filtered[i - 4][-2].split('_')[0]
                else:
                    story = trial_type + '_' + results_filtered[i - 2][-2].split('_')[1]
                reaction_time = target

                response_a = results_filtered[i-4][-1]
                response_b = results_filtered[i-3][-1]
                response_c = results_filtered[i-2][-1]
                response_d = results_filtered[i-1][-1]

                response_a = convert_to_binary(response_a)
                response_b = convert_to_binary(response_b)
                response_c = convert_to_binary(response_c)
                response_d = convert_to_binary(response_d)

                report_a = 'NA'
                report_b ='NA'
                report_c = 'NA'

                tag_a, tag_b, tag_c = ['NA']*3

                exhaustive = 'NA'

                #native_lang added here
                dataline = [subject, native_lang, age, gender, trial, story, reaction_time, response_a, response_b, response_c,
                            response_d, report_a, report_b, report_c, tag_a, tag_b, tag_c,exhaustive,mention_one_high,mention_one_low,
                            mention_some_high,mention_some_low,false_report,none_of_above,exhaustive_chosen,mention_one_high_chosen,mention_one_low_chosen,
                            mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen]
                print dataline
                pilot_results.writerow(dataline)

            # I had also been trying to wrtie code myself to process some new trials,
            #but in the end i just removed those trials in R and left that for future
             # if trial_type == 'nst':
             #    trial = trial_type
             #    story = trial_type + '_' + results_filtered[i - 4][-2].split('_')[0]

             #    story = 'NA'
             #    reaction_time = target

            if trial_type == 'exit_quest':
                trial = trial_type
                # story == trial_type + respondent prefix
                story = 'NA'
                reaction_time = target

                response_a = results_filtered[i-3][-1]
                response_b = results_filtered[i-2][-1]
                response_c = results_filtered[i-1][-1]
                response_d = 'NA'


                report_a = 'NA'
                report_b = 'NA'
                report_c = 'NA'
                tag_a, tag_b, tag_c = ['NA'] * 3

                exhaustive = 'NA'
                mention_one_high = 'NA'
                mention_one_low = 'NA'
                mention_some_high = 'NA'
                mention_some_low = 'NA'
                false_report = 'NA'
                none_of_above = 'NA'

                exhaustive_chosen = 'NA'
                mention_one_high_chosen = 'NA'
                mention_one_low_chosen = 'NA'
                mention_some_high_chosen = 'NA'
                mention_some_low_chosen = 'NA'
                false_report_chosen = 'NA'
                none_of_above_chosen = 'NA'

                #native_lang added here
                dataline = [subject, native_lang, age, gender, trial, story, reaction_time, response_a, response_b, response_c,
                            response_d, report_a, report_b, report_c, tag_a, tag_b, tag_c,exhaustive,mention_one_high,mention_one_low,
                            mention_some_high,mention_some_low,false_report,none_of_above,exhaustive_chosen,mention_one_high_chosen,
                            mention_one_low_chosen,mention_some_high_chosen,mention_some_low_chosen,false_report_chosen,none_of_above_chosen]
                print dataline
                pilot_results.writerow(dataline)
