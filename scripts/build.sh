cd ../pages

python -u compile.py Calculator.elm
python -u compile.py CountMeIn.elm
python -u compile.py Donate.elm
python -u compile.py Endorsements.elm
python -u compile.py Index.elm
python -u compile.py Map.elm
python -u compile.py Proposal.elm
python -u compile.py ReadMore.elm
python -u compile.py StateResults.elm

cd ../static/sass

scss style.scss style.css
scss 404.scss 404.css

cd ../../

python -u makeResults.py
