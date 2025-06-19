// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'Dotfiles',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/jozhw/.dotfiles' }],
			sidebar: [{
					label: 'Overview',
					autogenerate: { directory: 'overview' },
				},
				{
					label: 'Atzlan',
					autogenerate: { directory: 'atzlan' },
				},

				{
					label: 'Starlight',
					items: [
						// Each item here is one entry in the navigation menu.
						{ label: 'Starlight Guide', slug: 'starlight/guide' },
					],
				},
				{
					label: 'Dotfiles',
					autogenerate: { directory: 'dotfiles' },
				},
				{
					label: 'Garage',
					autogenerate: { directory: 'garage' },
				},
				{
					label: 'Reference',
					autogenerate: { directory: 'reference' },
				},
			],
		}),
	],
});
