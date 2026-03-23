#!/bin/bash
# Monthly cron wrapper: pull latest code, regenerate cache, push changes.
# Crontab: 0 3 1 * * /opt/INEbaseR/scripts/run_cache_update.sh

REPO_DIR="/opt/INEbaseR"
LOG_FILE="$REPO_DIR/cache_vps.log"
SCRIPT="$REPO_DIR/scripts/update_cache_vps.R"

exec >> "$LOG_FILE" 2>&1

echo ""
echo "=========================================="
echo "Cache update started: $(date '+%Y-%m-%d %H:%M:%S')"
echo "=========================================="

cd "$REPO_DIR" || { echo "ERROR: cannot cd to $REPO_DIR"; exit 1; }

# Pull latest changes from remote
echo "[git pull]"
git pull origin master

# Run the full recache
echo "[Rscript update_cache_vps.R]"
Rscript "$SCRIPT"

# Commit and push if any RDS files changed
CHANGED=$(git status --porcelain inst/extdata/ | grep -c '\.rds')
if [ "$CHANGED" -gt 0 ]; then
  echo "[git commit + push] $CHANGED RDS files changed"
  git add inst/extdata/SERIEOPERATION-*.rds
  git commit -m "Cache: actualización automática mensual $(date '+%Y-%m')"
  git push origin master
else
  echo "[git] No RDS changes to commit"
fi

echo "Cache update finished: $(date '+%Y-%m-%d %H:%M:%S')"
echo ""
