"""Compare original and generated screenshots without tolerance or masks.

Use --dark after browser_theme.js, --outline after browser_outline.js,
or --controls after browser_controls.js for viewport gates.
Requires Pillow in the test environment; it is not a Subux runtime dependency.
A nonzero exit code means the requested visual fidelity is not achieved.
"""
import json
from pathlib import Path
import sys
import argparse
from PIL import Image, ImageChops

parser = argparse.ArgumentParser(description=__doc__)
mode = parser.add_mutually_exclusive_group(required=True)
mode.add_argument('--dark', action='store_true')
mode.add_argument('--outline', action='store_true')
mode.add_argument('--controls', action='store_true')
parser.add_argument('root', nargs='?', default='output/playwright', type=Path)
args = parser.parse_args()
dark = args.dark
root = args.root
sites = ('tips', 'tips-doc', 'blog')
prefix = 'controls-' if args.controls else 'outline-' if args.outline else 'dark-'
cases = [(site, width, f'{prefix}{site}-reference-{width}', f'{prefix}{site}-subux-{width}', f'{prefix}{site}-diff-{width}')
         for site in sites for width in (1280, 390)]
if args.outline:
    cases = [(f'{theme}-{y}', 390, f'outline-{theme}-{y}-reference', f'outline-{theme}-{y}-subux', f'outline-{theme}-{y}-diff')
             for theme in ('light', 'dark') for y in (0, 32, 750)]
if args.controls:
    cases = [(f'{theme}-{state}', 390, f'controls-{theme}-{state}-4175', f'controls-{theme}-{state}-4173', f'controls-{theme}-{state}-diff')
             for theme in ('light', 'dark') for state in ('navigation', 'sidebar')]
results = []
for site, width, reference, generated, difference in cases:
    reference_path = root / f'{reference}.png'
    generated_path = root / f'{generated}.png'
    if not reference_path.exists() or not generated_path.exists():
        results.append(dict(site=site, width=width, error='Missing screenshot'))
        continue
    with Image.open(reference_path) as ref, Image.open(generated_path) as actual:
        if ref.size != (width, 900) or actual.size != ref.size:
            results.append(dict(site=site, width=width, error='Image dimensions differ'))
            continue
        diff = ImageChops.difference(ref.convert('RGB'), actual.convert('RGB'))
        channels = diff.split()
        mask = ImageChops.lighter(ImageChops.lighter(channels[0], channels[1]), channels[2])
        changed = ref.width * ref.height - mask.histogram()[0]
        diff.save(root / f'{difference}.png')
        results.append(dict(site=site, width=width, pixels=ref.width * ref.height,
                            different_pixels=changed, identical=changed == 0))
(root / f'{prefix}pixel-report.json').write_text(json.dumps(results, indent=2) + '\n')
print(json.dumps(results, indent=2))
sys.exit(0 if all(item.get('identical', False) for item in results) else 1)
