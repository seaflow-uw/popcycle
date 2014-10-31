crontab -e
*/60 * * * * ~/data_backup.sh >~/data_backup.log 2>&1
*/3 * * * * ~/RT_analysis.sh >~/RT_analysis.log 2>&1