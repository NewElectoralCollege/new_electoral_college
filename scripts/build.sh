cd ../static/sass

scss calculator.scss calculator.css
scss style.scss countmein.css
scss style.scss donatefaq.css
scss donate.scss donate.css
scss endorsements.scss endorsements.css
scss index.scss index.css
scss map.scss map.css
scss proposal.scss proposal.css
scss readmore.scss readmore.css
scss stateresults.scss stateresults.css
scss error.scss pagenotfound.css
scss error.scss notauthorized.css

cd ../../pages

python -u compile.py PageNotFound.elm
python -u compile.py NotAuthorized.elm
python -u compile.py Calculator.elm
python -u compile.py CountMeIn.elm
python -u compile.py Donate.elm
python -u compile.py DonateFAQ.elm
python -u compile.py Endorsements.elm
python -u compile.py Index.elm
python -u compile.py Map.elm
python -u compile.py Proposal.elm
python -u compile.py ReadMore.elm
python -u compile.py StateResults.elm
