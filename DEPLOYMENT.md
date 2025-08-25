# GitHub Pages Deployment Instructions

## Automatic Deployment (Recommended)

The pkgdown website will be automatically deployed to GitHub Pages whenever you push to the main branch, thanks to the GitHub Actions workflow (`.github/workflows/pkgdown.yaml`).

### To enable automatic deployment:

1. **Push your changes to GitHub:**
   ```bash
   git add .
   git commit -m "Add pkgdown website configuration"
   git push origin main
   ```

2. **Enable GitHub Pages in your repository:**
   - Go to your GitHub repository: https://github.com/Divendra2006/polarisR
   - Click on "Settings" tab
   - Scroll down to "Pages" in the left sidebar
   - Under "Source", select "GitHub Actions"
   - The workflow will automatically trigger and deploy your site

3. **Your website will be available at:**
   https://divendra2006.github.io/polarisR/

## Manual Deployment

If you prefer to build and deploy manually:

```bash
# Build the site locally
R -e "pkgdown::build_site()"

# The built site will be in the docs/ folder
# You can then push it to the gh-pages branch or use GitHub Pages source
```

## Features of Your pkgdown Website

✅ **Homepage**: Built from README.md with package description
✅ **Function Reference**: Auto-generated documentation for all exported functions
✅ **Articles/Vignettes**: Your "Introduction to polarisR" vignette is linked and accessible
✅ **Search**: Full-text search across all documentation
✅ **Responsive Design**: Works on mobile and desktop
✅ **Bootstrap Theme**: Uses the sandstone theme for a professional look
✅ **Favicons**: Auto-generated favicons from your logo
✅ **Author Information**: Links to GitHub profiles and ORCIDs

## Customization

You can further customize your website by editing `_pkgdown.yml`:
- Change themes, colors, and layout
- Add more navigation items
- Include additional articles or tutorials
- Modify the homepage content

## Maintenance

The website will automatically update when you:
- Push changes to the main branch
- Update documentation in your R files
- Modify vignettes
- Update the README.md file

Your pkgdown website is now ready and configured! 🎉
