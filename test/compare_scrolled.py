"""Compare every unaltered viewport capture from browser_scrolled.js, RGB exact."""
import json
from pathlib import Path
import sys
import argparse
from PIL import Image, ImageChops

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('manifest', nargs='?', type=Path, default=Path('output/scroll-manifest.json'))
parser.add_argument('--full', action='store_true', help='Require all 17 routes at both target widths in light mode')
args = parser.parse_args()
manifest_path = args.manifest
cases = json.loads(manifest_path.read_text())
if not cases:
    sys.exit('No viewport captures in manifest')
coverage = {}
for case in cases:
    coverage.setdefault((case['name'], case['width'], case.get('colorScheme', 'light')), []).append(case)
if args.full:
    routes = ['tips', 'tips-linux', 'tips-git', *(f'blog-{i}' for i in range(14))]
    expected = {(route, width, 'light') for route in routes for width in (1280, 390)}
    if set(coverage) != expected:
        sys.exit(f'Full matrix scope mismatch: missing={sorted(expected - set(coverage))}, extra={sorted(set(coverage) - expected)}')
    environments = {json.dumps(case.get('environment'), sort_keys=True) for case in cases}
    if len(environments) != 1:
        sys.exit('Full matrix mixes renderer environments')
for name, captures in coverage.items():
    heights = {case['height'] for case in captures}
    offsets = sorted(case['y'] for case in captures)
    if len(heights) != 1 or len(set(offsets)) != len(offsets):
        sys.exit(f'{name}: inconsistent or duplicate captures')
    if offsets[0] != 0 or offsets[-1] != max(0, next(iter(heights)) - 900) or any(b - a > 900 for a, b in zip(offsets, offsets[1:])):
        sys.exit(f'{name}: viewport coverage has gaps')
results = []
for case in cases:
    with Image.open(case['files'][0]) as original, Image.open(case['files'][1]) as actual:
        result = {key: case[key] for key in ('name', 'width', 'y', 'height')}
        if original.size != (case['width'], 900) or actual.size != original.size:
            result['error'] = 'Unexpected viewport dimensions'
        else:
            diff = ImageChops.difference(original.convert('RGB'), actual.convert('RGB'))
            red, green, blue = diff.split()
            mask = ImageChops.lighter(ImageChops.lighter(red, green), blue)
            result['different_pixels'] = original.width * original.height - mask.histogram()[0]
            result['identical'] = result['different_pixels'] == 0
            if not result['identical']:
                diff.save(Path(case['files'][1]).with_name(Path(case['files'][1]).stem + '-diff.png'))
        results.append(result)
manifest_path.with_name(manifest_path.stem.removesuffix('-manifest') + '-pixel-report.json').write_text(json.dumps(results, indent=2) + '\n')
print(f"Identical: {sum(case.get('identical', False) for case in results)}/{len(results)}")
print(json.dumps([case for case in results if not case.get('identical')], indent=2))
sys.exit(0 if all(case.get('identical') for case in results) else 1)
